# Reactive channel helpers built on nanonext.
#
# This vendors the minimal async channel layer needed in this app:
# - event-driven worker -> Shiny updates
# - Shiny -> worker cancellation broadcasts
# - async progress updates

.kwallm_queue_registry <- new.env(parent = emptyenv())
.kwallm_interrupt_registry <- new.env(parent = emptyenv())
.kwallm_async_progress_registry <- new.env(parent = emptyenv())

.kwallm_generate_id <- function() {
  paste0(
    sprintf("%02x", sample.int(256L, 16L, replace = TRUE) - 1L),
    collapse = ""
  )
}

.kwallm_generate_token <- function() {
  paste0(
    sprintf("%02x", sample.int(256L, 32L, replace = TRUE) - 1L),
    collapse = ""
  )
}

.kwallm_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

.kwallm_validate_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
    rlang::abort(
      "`url` must be a single non-empty character string.",
      class = "nr_invalid_url"
    )
  }

  invisible(url)
}

.kwallm_extract_ws_url <- function(url) {
  sub("\\?.*$", "", url)
}

.kwallm_extract_pub_url <- function(url) {
  match <- regmatches(url, regexpr("(?<=\\?pub=).*$", url, perl = TRUE))
  if (length(match) == 0L || !nzchar(match)) {
    return(NULL)
  }

  utils::URLdecode(match)
}

.kwallm_find_free_pub_socket <- function(tls = NULL) {
  transport <- if (!is.null(tls)) "tls+tcp" else "tcp"

  for (i in seq_len(100L)) {
    port <- sample(49152L:65535L, 1L)
    url <- paste0(transport, "://127.0.0.1:", port)

    socket <- tryCatch(
      nanonext::socket("pub", listen = url, tls = tls),
      error = function(e) NULL
    )

    if (!is.null(socket)) {
      attr(socket, "url") <- url
      return(socket)
    }
  }

  rlang::abort(
    "Could not find a free TCP port for the pub/sub broadcast socket.",
    class = "nr_port_failed"
  )
}


# Core channel ---------------------------------------------------------------

nr_channel <- function(
  session = NULL,
  initial_value = NULL,
  throttle = 0.1,
  tls = NULL
) {
  if (
    !is.numeric(throttle) ||
      length(throttle) != 1L ||
      is.na(throttle) ||
      throttle < 0
  ) {
    rlang::abort(
      "`throttle` must be a single non-negative number (seconds).",
      class = "nr_invalid_throttle"
    )
  }

  tls_client <- NULL
  server_tls <- NULL

  if (isTRUE(tls)) {
    tls <- nanonext::write_cert(cn = "127.0.0.1")
  }

  if (is.list(tls)) {
    if (is.null(tls$server)) {
      rlang::abort(
        paste0(
          "`tls` must be `TRUE` (auto-generate) or a list with `$server` ",
          "and `$client` components."
        ),
        class = "nr_invalid_tls"
      )
    }

    server_tls <- nanonext::tls_config(server = tls$server)
    tls_client <- tls$client
  } else if (!is.null(tls)) {
    rlang::abort(
      paste0(
        "`tls` must be `NULL`, `TRUE`, or a list with `$server` and ",
        "`$client` components."
      ),
      class = "nr_invalid_tls"
    )
  }

  use_tls <- !is.null(server_tls)
  id <- .kwallm_generate_id()
  token <- .kwallm_generate_token()
  open <- TRUE
  clients <- new.env(parent = emptyenv())
  subscribers <- new.env(parent = emptyenv())

  pending_raw <- NULL
  timer_active <- FALSE
  last_flush_time <- 0

  on_message <- function(ws, data, ...) {
    if (!open) {
      return()
    }

    pending_raw <<- data

    if (!timer_active) {
      timer_active <<- TRUE

      now <- proc.time()[["elapsed"]]
      elapsed <- now - last_flush_time
      delay <- if (elapsed >= throttle) 0 else throttle - elapsed

      later::later(
        function() {
          timer_active <<- FALSE

          if (is.null(pending_raw) || !open) {
            return()
          }

          raw <- pending_raw
          pending_raw <<- NULL
          last_flush_time <<- proc.time()[["elapsed"]]

          value <- unserialize(raw)
          if (nanonext::is_error_value(value)) {
            return()
          }

          subscriber_ids <- ls(subscribers, sorted = FALSE)
          for (subscriber_id in subscriber_ids) {
            subscriber <- get(
              subscriber_id,
              envir = subscribers,
              inherits = FALSE
            )
            subscriber$rv(value)
          }
        },
        delay = delay
      )
    }
  }

  on_open <- function(ws, ...) {
    assign(as.character(ws$id), ws, envir = clients)
  }

  on_close <- function(ws, ...) {
    client_id <- as.character(ws$id)
    if (exists(client_id, envir = clients, inherits = FALSE)) {
      rm(list = client_id, envir = clients)
    }
  }

  server <- nanonext::http_server(
    url = paste0(if (use_tls) "https" else "http", "://127.0.0.1:0"),
    handlers = nanonext::handler_ws(
      paste0("/ws/", token),
      on_message = on_message,
      on_open = on_open,
      on_close = on_close
    ),
    tls = server_tls
  )
  server$start()

  pub_socket <- .kwallm_find_free_pub_socket(tls = server_tls)
  pub_url <- attr(pub_socket, "url")

  ws_url <- sub("^https://", "wss://", sub("^http://", "ws://", server$url))
  ws_url <- paste0(ws_url, "/ws/", token)
  url <- paste0(ws_url, "?pub=", utils::URLencode(pub_url, reserved = TRUE))

  send_to_workers <- function(value) {
    if (!open) {
      rlang::abort(
        "Cannot send on a closed channel.",
        class = "nr_channel_closed"
      )
    }

    raw_data <- serialize(value, NULL)
    nanonext::send(pub_socket, raw_data, mode = "raw")
    invisible(NULL)
  }

  subscribe <- function(
    session = shiny::getDefaultReactiveDomain(),
    initial_value = NULL
  ) {
    if (is.null(session)) {
      rlang::abort(
        paste0(
          "No Shiny session found. `subscribe()` must be called inside ",
          "a Shiny server function."
        ),
        class = "nr_no_session"
      )
    }

    if (!open) {
      rlang::abort(
        "Cannot subscribe to a closed channel.",
        class = "nr_channel_closed"
      )
    }

    subscription_id <- .kwallm_generate_id()
    rv <- shiny::reactiveVal(initial_value)
    assign(subscription_id, list(rv = rv), envir = subscribers)

    unsubscribe <- function() {
      if (exists(subscription_id, envir = subscribers, inherits = FALSE)) {
        rm(list = subscription_id, envir = subscribers)
      }

      invisible(NULL)
    }

    session$onSessionEnded(unsubscribe)

    structure(
      list(
        id = subscription_id,
        value = rv,
        unsubscribe = unsubscribe
      ),
      class = "nr_subscription"
    )
  }

  close_channel <- function() {
    if (!open) {
      return(invisible(NULL))
    }

    open <<- FALSE

    subscriber_ids <- ls(subscribers, sorted = FALSE)
    if (length(subscriber_ids)) {
      rm(list = subscriber_ids, envir = subscribers)
    }

    client_ids <- ls(clients, sorted = FALSE)
    if (length(client_ids)) {
      rm(list = client_ids, envir = clients)
    }

    tryCatch(server$close(), error = function(e) NULL)
    tryCatch(close(pub_socket), error = function(e) NULL)

    invisible(NULL)
  }

  auto_value <- NULL
  if (!is.null(session)) {
    auto_subscription <- subscribe(
      session = session,
      initial_value = initial_value
    )
    auto_value <- auto_subscription$value
    session$onSessionEnded(close_channel)
  }

  structure(
    list(
      id = id,
      url = url,
      value = auto_value,
      subscribe = subscribe,
      send = send_to_workers,
      clients = clients,
      subscribers = subscribers,
      tls = tls_client,
      close = close_channel
    ),
    class = "nr_channel"
  )
}


# Worker helpers -------------------------------------------------------------

local({
  connection_cache <- new.env(parent = emptyenv())

  extract_ws_url <- function(url) {
    sub("\\?.*$", "", url)
  }

  extract_pub_url <- function(url) {
    match <- regmatches(url, regexpr("(?<=\\?pub=).*$", url, perl = TRUE))
    if (length(match) == 0L || !nzchar(match)) {
      return(NULL)
    }

    utils::URLdecode(match)
  }

  nr_send <<- function(url, value, tls = NULL) {
    if (inherits(url, "nr_connection")) {
      stream <- url$stream
      url_label <- url$url
    } else if (is.character(url) && length(url) == 1L && nzchar(url)) {
      ws_url <- extract_ws_url(url)
      cache_key <- paste0("send:", url)

      if (exists(cache_key, envir = connection_cache, inherits = FALSE)) {
        conn <- get(cache_key, envir = connection_cache, inherits = FALSE)
      } else {
        stream_tls <- tls
        if (is.null(stream_tls) && startsWith(ws_url, "wss://")) {
          stream_tls <- nanonext::tls_config()
        }

        ws <- tryCatch(
          nanonext::stream(dial = ws_url, tls = stream_tls),
          error = function(e) {
            rlang::abort(
              paste0(
                "Failed to connect to channel at '",
                ws_url,
                "': ",
                conditionMessage(e)
              ),
              class = "nr_connect_failed",
              parent = e
            )
          }
        )

        conn <- list(stream = ws, url = url)
        assign(cache_key, conn, envir = connection_cache)
      }

      stream <- conn$stream
      url_label <- url
    } else {
      rlang::abort(
        paste0(
          "`url` must be a channel URL string or an object returned by ",
          "`nr_connect()`."
        ),
        class = "nr_invalid_target"
      )
    }

    result <- nanonext::send(stream, serialize(value, NULL), mode = "raw")

    if (nanonext::is_error_value(result)) {
      rlang::abort(
        paste0(
          "Failed to send data to '",
          url_label,
          "'. The channel may be closed."
        ),
        class = "nr_send_failed"
      )
    }

    invisible(result)
  }

  nr_receive <<- function(url, tls = NULL) {
    if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
      rlang::abort(
        "`url` must be a channel URL string (from `nr_channel()$url`).",
        class = "nr_invalid_target"
      )
    }

    pub_url <- extract_pub_url(url)
    if (is.null(pub_url)) {
      rlang::abort(
        paste0(
          "Channel URL does not contain a pub/sub broadcast address. ",
          "Make sure you are using a URL from `nr_channel()$url`."
        ),
        class = "nr_no_pub_url"
      )
    }

    cache_key <- paste0("recv:", url)

    if (exists(cache_key, envir = connection_cache, inherits = FALSE)) {
      conn <- get(cache_key, envir = connection_cache, inherits = FALSE)
    } else {
      recv_tls <- tls
      if (is.null(recv_tls) && startsWith(pub_url, "tls+tcp://")) {
        recv_tls <- nanonext::tls_config()
      }

      socket <- tryCatch(
        nanonext::socket("sub", dial = pub_url, tls = recv_tls),
        error = function(e) {
          rlang::abort(
            paste0(
              "Failed to connect to broadcast channel at '",
              pub_url,
              "': ",
              conditionMessage(e)
            ),
            class = "nr_connect_failed",
            parent = e
          )
        }
      )

      nanonext::subscribe(socket, topic = NULL)
      conn <- list(stream = socket, url = url)
      assign(cache_key, conn, envir = connection_cache)
    }

    result <- nanonext::recv(conn$stream, mode = "raw", block = 0L)

    if (nanonext::is_error_value(result)) {
      return(NULL)
    }

    unserialize(result)
  }

  nr_connect <<- function(url, tls = NULL) {
    .kwallm_validate_url(url)

    ws_url <- extract_ws_url(url)
    stream_tls <- tls
    if (is.null(stream_tls) && startsWith(ws_url, "wss://")) {
      stream_tls <- nanonext::tls_config()
    }

    stream <- tryCatch(
      nanonext::stream(dial = ws_url, tls = stream_tls),
      error = function(e) {
        rlang::abort(
          paste0(
            "Failed to connect to channel at '",
            ws_url,
            "': ",
            conditionMessage(e)
          ),
          class = "nr_connect_failed",
          parent = e
        )
      }
    )

    structure(
      list(stream = stream, url = url),
      class = "nr_connection"
    )
  }

  nr_disconnect <<- function(url) {
    if (inherits(url, "nr_connection")) {
      tryCatch(close(url$stream), error = function(e) NULL)
    } else if (is.character(url) && length(url) == 1L && nzchar(url)) {
      keys <- c(paste0("send:", url), paste0("recv:", url))
      for (key in keys) {
        if (!exists(key, envir = connection_cache, inherits = FALSE)) {
          next
        }

        conn <- get(key, envir = connection_cache, inherits = FALSE)
        tryCatch(close(conn$stream), error = function(e) NULL)
        rm(list = key, envir = connection_cache)
      }
    } else {
      rlang::abort(
        paste0(
          "`url` must be an `nr_connection` object or a channel URL string."
        ),
        class = "nr_invalid_target"
      )
    }

    invisible(NULL)
  }
})

print.nr_channel <- function(x, ...) {
  cat("<nr_channel>", x$id, "\n")
  cat("  url:", x$url, "\n")
  cat("  tls:", if (is.null(x$tls)) "off" else "on", "\n")
  if (!is.null(x$value)) {
    cat("  value: <reactiveVal>\n")
  }
  cat("  subscribers:", length(ls(x$subscribers)), "\n")
  cat("  clients:", length(ls(x$clients)), "\n")
  invisible(x)
}

print.nr_subscription <- function(x, ...) {
  cat("<nr_subscription>", x$id, "\n")
  cat("  value: <reactiveVal>\n")
  invisible(x)
}

print.nr_connection <- function(x, ...) {
  cat("<nr_connection>\n")
  cat("  url:", x$url, "\n")
  invisible(x)
}


# Queue compatibility --------------------------------------------------------

.kwallm_dispatch_queue_message <- function(target_env, payload) {
  if (!is.list(payload)) {
    return(invisible(NULL))
  }

  assignments <- NULL
  if (identical(payload$type, "assign_reactive")) {
    reactive_name <- payload$name
    if (!is.character(reactive_name) || length(reactive_name) != 1L) {
      return(invisible(NULL))
    }

    assignments <- stats::setNames(list(payload$value), reactive_name)
  } else if (identical(payload$type, "assign_reactive_batch")) {
    assignments <- payload$values
    if (!is.list(assignments) || is.null(names(assignments))) {
      return(invisible(NULL))
    }
  } else {
    return(invisible(NULL))
  }

  for (reactive_name in names(assignments)) {
    if (!exists(reactive_name, envir = target_env, inherits = TRUE)) {
      next
    }

    reactive_value <- get(reactive_name, envir = target_env, inherits = TRUE)
    if (!is.function(reactive_value)) {
      next
    }

    try(reactive_value(assignments[[reactive_name]]), silent = TRUE)
  }

  invisible(NULL)
}

.kwallm_queue_start <- function(id) {
  if (!exists(id, envir = .kwallm_queue_registry, inherits = FALSE)) {
    return(invisible(NULL))
  }

  entry <- get(id, envir = .kwallm_queue_registry, inherits = FALSE)
  if (!is.null(entry$observer)) {
    return(invisible(NULL))
  }

  entry$observer <- shiny::observe(
    {
      payload <- entry$channel$value()
      if (is.null(payload)) {
        return()
      }

      .kwallm_dispatch_queue_message(entry$target_env, payload)
    },
    domain = entry$session
  )

  assign(id, entry, envir = .kwallm_queue_registry)
  invisible(NULL)
}

.kwallm_queue_stop <- function(id) {
  if (!exists(id, envir = .kwallm_queue_registry, inherits = FALSE)) {
    return(invisible(NULL))
  }

  entry <- get(id, envir = .kwallm_queue_registry, inherits = FALSE)
  if (!is.null(entry$observer)) {
    try(entry$observer$destroy(), silent = TRUE)
    entry$observer <- NULL
    assign(id, entry, envir = .kwallm_queue_registry)
  }

  invisible(NULL)
}

.kwallm_queue_cleanup <- function(id) {
  if (!exists(id, envir = .kwallm_queue_registry, inherits = FALSE)) {
    return(invisible(NULL))
  }

  entry <- get(id, envir = .kwallm_queue_registry, inherits = FALSE)
  .kwallm_queue_stop(id)
  try(entry$channel$close(), silent = TRUE)
  try(nr_disconnect(entry$channel$url), silent = TRUE)
  rm(list = id, envir = .kwallm_queue_registry)

  invisible(NULL)
}

shinyQueue <- function(
  session = shiny::getDefaultReactiveDomain(),
  throttle = 0.05
) {
  if (is.null(session)) {
    rlang::abort(
      "`shinyQueue()` must be called inside a Shiny session.",
      class = "kwallm_no_shiny_session"
    )
  }

  queue_id <- .kwallm_generate_id()
  channel <- nr_channel(session = session, throttle = throttle)

  assign(
    queue_id,
    list(
      channel = channel,
      target_env = parent.frame(),
      session = session,
      observer = NULL
    ),
    envir = .kwallm_queue_registry
  )

  send_fn <- nr_send
  queue_url <- channel$url
  producer_state <- new.env(parent = emptyenv())
  producer_state$values <- list()

  session$onSessionEnded(function() {
    .kwallm_queue_cleanup(queue_id)
  })

  queue <- structure(
    list(
      id = queue_id,
      url = channel$url,
      producer = list(
        fireAssignReactive = function(name, value) {
          producer_state$values[[name]] <- value

          send_fn(
            queue_url,
            list(
              type = "assign_reactive_batch",
              values = producer_state$values
            )
          )

          invisible(NULL)
        }
      ),
      consumer = list(
        start = function(millis = 100) {
          invisible(millis)
          .kwallm_queue_start(queue_id)
        },
        stop = function() {
          .kwallm_queue_stop(queue_id)
        }
      )
    ),
    class = "Queue"
  )

  queue
}


# Interrupt compatibility ----------------------------------------------------

.kwallm_interrupt_cleanup <- function(id) {
  if (!exists(id, envir = .kwallm_interrupt_registry, inherits = FALSE)) {
    return(invisible(NULL))
  }

  entry <- get(id, envir = .kwallm_interrupt_registry, inherits = FALSE)
  try(entry$channel$close(), silent = TRUE)
  try(nr_disconnect(entry$url), silent = TRUE)
  rm(list = id, envir = .kwallm_interrupt_registry)

  invisible(NULL)
}

AsyncInterruptor <- list(
  new = function(session = shiny::getDefaultReactiveDomain()) {
    channel <- nr_channel(session = session, throttle = 0)
    interruptor_id <- .kwallm_generate_id()
    send_fn <- nr_send
    receive_fn <- nr_receive
    url <- channel$url

    assign(
      interruptor_id,
      list(channel = channel, url = url),
      envir = .kwallm_interrupt_registry
    )

    if (!is.null(session)) {
      session$onSessionEnded(function() {
        .kwallm_interrupt_cleanup(interruptor_id)
      })
    }

    structure(
      list(
        id = interruptor_id,
        url = url,
        interrupt = function(message = "Interrupted") {
          if (
            !exists(
              interruptor_id,
              envir = .kwallm_interrupt_registry,
              inherits = FALSE
            )
          ) {
            return(invisible(NULL))
          }

          entry <- get(
            interruptor_id,
            envir = .kwallm_interrupt_registry,
            inherits = FALSE
          )
          entry$channel$send(list(
            type = "interrupt",
            reason = as.character(message)[1]
          ))

          invisible(NULL)
        },
        execInterrupts = function() {
          payload <- receive_fn(url)
          if (!is.list(payload) || !identical(payload$type, "interrupt")) {
            return(invisible(NULL))
          }

          reason <- payload$reason
          if (
            !is.character(reason) || length(reason) != 1L || !nzchar(reason)
          ) {
            reason <- "Interrupted"
          }

          stop(structure(
            list(message = reason),
            class = c("kwallm_async_interrupt", "error", "condition")
          ))
        },
        destroy = function() {
          .kwallm_interrupt_cleanup(interruptor_id)
          invisible(NULL)
        }
      ),
      class = "AsyncInterruptor"
    )
  }
)


# Async progress compatibility -----------------------------------------------

.kwallm_async_progress_cleanup <- function(id) {
  if (!exists(id, envir = .kwallm_async_progress_registry, inherits = FALSE)) {
    return(invisible(NULL))
  }

  entry <- get(id, envir = .kwallm_async_progress_registry, inherits = FALSE)

  if (!is.null(entry$observer)) {
    try(entry$observer$destroy(), silent = TRUE)
  }

  if (!is.null(entry$progress)) {
    try(entry$progress$close(), silent = TRUE)
  }

  try(entry$channel$close(), silent = TRUE)
  try(nr_disconnect(entry$url), silent = TRUE)
  rm(list = id, envir = .kwallm_async_progress_registry)

  invisible(NULL)
}

AsyncProgress <- list(
  new = function(
    message = NULL,
    detail = NULL,
    session = shiny::getDefaultReactiveDomain(),
    throttle = 0.05
  ) {
    if (is.null(session)) {
      rlang::abort(
        "`AsyncProgress$new()` must be called inside a Shiny session.",
        class = "kwallm_no_shiny_session"
      )
    }

    channel <- nr_channel(session = session, throttle = throttle)
    progress_id <- .kwallm_generate_id()
    send_fn <- nr_send
    progress_url <- channel$url
    progress <- shiny::Progress$new(session = session)
    progress$set(value = 0, message = message, detail = detail)

    state <- new.env(parent = emptyenv())
    state$value <- 0
    worker_state <- new.env(parent = emptyenv())
    worker_state$value <- 0

    observer <- shiny::observe(
      {
        payload <- channel$value()
        if (is.null(payload) || !is.list(payload)) {
          return()
        }

        if (identical(payload$type, "set_progress")) {
          value <- suppressWarnings(as.numeric(payload$value))
          if (length(value) != 1L || is.na(value)) {
            value <- state$value
          }

          state$value <- max(0, min(1, value))
          progress$set(
            value = state$value,
            message = .kwallm_coalesce(payload$message, message),
            detail = .kwallm_coalesce(payload$detail, detail)
          )
        } else if (identical(payload$type, "inc")) {
          amount <- suppressWarnings(as.numeric(payload$amount))
          if (length(amount) != 1L || is.na(amount)) {
            amount <- 0
          }

          state$value <- max(0, min(1, state$value + amount))
          progress$set(
            value = state$value,
            message = .kwallm_coalesce(payload$message, message),
            detail = .kwallm_coalesce(payload$detail, detail)
          )
        }
      },
      domain = session
    )

    assign(
      progress_id,
      list(
        channel = channel,
        url = channel$url,
        progress = progress,
        observer = observer
      ),
      envir = .kwallm_async_progress_registry
    )

    session$onSessionEnded(function() {
      .kwallm_async_progress_cleanup(progress_id)
    })

    structure(
      list(
        id = progress_id,
        url = progress_url,
        inc = function(amount, detail = NULL, message = NULL) {
          if (!is.numeric(amount) || length(amount) != 1L || is.na(amount)) {
            return(invisible(NULL))
          }

          worker_state$value <- max(0, min(1, worker_state$value + amount))

          send_fn(
            progress_url,
            list(
              type = "set_progress",
              value = worker_state$value,
              detail = detail,
              message = message
            )
          )

          invisible(NULL)
        },
        close = function() {
          .kwallm_async_progress_cleanup(progress_id)
          invisible(NULL)
        }
      ),
      class = "AsyncProgress"
    )
  }
)
