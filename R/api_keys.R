set_api_key <- function(service = "ecmwfr", user = NULL) {
  if (!interactive()) {
    cli::cli_abort(c(
      "Cannot open a key prompt in a non-interactive session.",
      "i" = "Please set the key interactively."
    ))
  }

  keyring::key_set(
    service,
    username = user,
    prompt = sprintf("Enter your API key for service", service)
  )
}


.check_api_key <- function(service) {
  switch(
    service,
    ecmwfr = with_cli(ecmwfr::wf_get_key())
  )
}


.ecmwfr_get_key <- function(service) {
  key <- Sys.getenv("ecmwfr_PAT")
  if (nchar(key) > 0) {
    return(key)
  }

  .get_key_from_keyring()
}


.get_key_from_keyring <- function(service, user = service, keyring = service) {
  if (!.keyring_is_default("env")) {
    if (.keyring_is_default("file") &&
        !service %in% keyring::keyring_list()$keyring) {
      cli::cli_abort("Cannot find credentials in the keyring file.")
    }

    .unlock_keyring("ecmwfr")
  }

  .check_has_key(service, user = user, keyring = keyring)
  if (.keyring_is_default("file")) {
    keyring::key_get(service = service, username = user, keyring = keyring)
  } else {
    keyring::key_get(service = service, username = user)
  }
}


.check_has_key <- function(service, user, keyring) {
  keys <- if (.keyring_is_default("file")) {
    keyring::key_list(service, keyring)
  } else {
    keyring::key_list(service)
  }

  if (!nrow(keys)) {
    service_fmt <- cli::col_green(dquote(service))
    cli::cli_abort(c(
      "No API key found for service {cli::col_blue(service)}.",
      "i" = "You can use `set_api_key({service_fmt})` to set an API key."
    ))
  }
}


.keyring_is_default <- function(name) {
  identical(keyring::default_backend()$name, name)
}


.unlock_keyring <- function(keyring) {
  if (keyring::keyring_is_locked(keyring = keyring)) {
    cli::cli_inform("Your keyring is locked. Please unlock with your keyring password.")
    keyring::keyring_unlock(keyring = keyring)
  }
}
