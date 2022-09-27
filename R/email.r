# email.r
#
# **Description ---------------------------------------------------------------
# Send an Outlook email.
# author - mtta17@gmail.com
#
# ***********************************************
# **************** thoughts *********************
# ***********************************************
#
# would be cool to add support for gmail too
#
# ***********************************************
#
# send_email()
#
# send_email() ################################################################
# **Arguments -----------------------------------------------------------------
# to				  string of email recipients. multiple email addresses should be separated by semi-colons
# subject             string of the subject line message
# body                string. This is an HTML body and will display any HTML that is embedded within the string
# attachment = ""     string vector to the paths of files to be attached
# cc = ""             string of recipients to cc. multiple email addresses should be separated by semi-colons
# bcc = ""            string of recipients to bcc. multiple email addresses should be separated by semi-colons
# encrypt = F         bool. If true, precedes the subject with "$ecure" to encrypt and send the message via the DataMotion Outlook plugin
# **Output --------------------------------------------------------------------
# Sends an email via Outlook; no returns in-session
# **Dependencies --------------------------------------------------------------
# RDCOMClient, purrr

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
} # end send_email()
