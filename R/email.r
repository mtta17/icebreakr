# email.r
# mtta17@gmail.com
#
#' Send an Outlook Email
#'
#' Email is sent from the Microsoft domain that is presently signed in on your computer's desktop Outlook application.
#'
#' @param to String of email recipients. Multiple email addresses should be separated by semi-colons
#' @param subject String of the subject line message
#' @param body String. This is an HTML body and will display any HTML that is embedded within the string
#' @param attachment = "" String vector to the paths of files to be attached
#' @param cc = "" String of recipients to cc. Multiple email addresses should be separated by semi-colons
#' @param bcc = "" String of recipients to bcc. Multiple email addresses should be separated by semi-colons
#' @param encrypt = F Bool. If true, precedes the subject with "$ecure" to encrypt and send the message via the DataMotion
#' @return Sends an email via Outlook; no returns in-session
#' @export
send_email <- function(to, subject, body, attachment = "", cc = "", bcc = "", encrypt = F){
    # Open Outlook
    Outlook <- RDCOMClient::COMCreate("Outlook.Application")
    # Create a new email object
    Email = Outlook$CreateItem(0)
    if(encrypt == T){
        subject <- paste0("$ecure ", subject)
    }
    # Set the recipient, subject, and body
    # multiple recepients should be written as a semi-colon separated string
    Email[["to"]] = to
    Email[["cc"]] = cc
    Email[["bcc"]] = bcc
    Email[["subject"]] = subject
    Email[["HTMLbody"]] = body
    purrr::map(attachment, ~Email[["attachments"]]$Add(.))
    # Send the message
    Email$Send()
    # Close Outlook, clear the message
    rm(Outlook, Email)
} # send_email()

# would be cool to add gmail support too
