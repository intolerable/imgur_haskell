`imgur_haskell`
===

Library for uploading images to [Imgur](http://imgur.com) and accessing them. Needs an API key from Imgur.

Uploading
---

Use `uploadFile`, `uploadImageData` or `copyFromUrl` to upload images. They all take a `ClientID` as their first arg (which is just a string) and all return `IO (Maybe Image)`s.