# Changelog

## 1.7

* Add support for Feed Categories
  * RSS: http://www.rssboard.org/rss-specification#ltcategorygtSubelementOfLtitemgt
  * Atom: https://tools.ietf.org/html/rfc4287#section-4.2.2
  * Create the `EntryCategory` datatype

## 1.6.1

* Upgrade to yesod-core 1.6.0

## 1.6

* Create new datatype `EntryEnclosure` for self-documentation of `feedEntryEnclosure`.

## 1.5

### Yesod/FeedTypes.hs

* added `feedLogo` field to `Feed` type as `Maybe (url, Text)`. Should this field result in `Nothing`, nothing will be added to the feed.
	* `url`: Defines the URL to the logi image
	* `Text`: Is the description of the logo image. Will only be used for RSS feeds
* added `feedEntryEnclosure` field to `FeedEntry` type as `Maybe (url, Int, Text)`. Should this field result in `Nothing`, no data will be added to the feed entry.
	* `url`: Defines the URL to the enclosed data
	* `Int`: Is the content size in bytes. RSS requires this.
	* `Text`: Is the MIME-type of the enclosed file. RSS requires this

### Yesod/AtomFeed.hs

* modified `template` function to append an `<logo>url</logo>` tag at the end of the feed, when the provided `feedLogo` is not `Nothing`. This might look awkward, since it will be appended *after* the entries.
* modified `entryTemplate` to append an `<link rel="enclosure" href=url>` to the feed entry.

### Yesod/RssFeed.hs

* modified `template` function to append an `<image>` tag with its three required components `<url>`, `<title>` and `<link>`. `<url>` and `<title>` will be filled from `feedLogo`, `<link>` will be `feedLinkHome`.
* modified `entryTemplate` function to append an `<enclosure type=mime length=length url=url>` to the feed entry
