library(twitteR)
library(ROAuth)
# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")


# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")
registerTwitterOAuth(cred)

consumer_key <- "iIqt5d6bpRKF37RaWdyqJLAWJ"

consumer_secret <- "qQc7iiYZddit7EBhnJEmJmTGSyDiK3MEIWqjW1x3CwJnmy7C6u"

access_key <- "396944590-saMC6Z5r1dtjLUkz7AHKQBCgIBlb9T7czf1XaCwK"

access_secret <- "QYldXZMWypffxe2zbqOf00Y92SyG9PdOkhdtfL9WVM0X7"