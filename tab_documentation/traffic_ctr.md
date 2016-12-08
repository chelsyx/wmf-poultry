Wikimedia Portal Traffic and Clickthrough Rate by Geography
=======

This dashboard shows where portal users come from each day, displaying the number of events, number of visits and the number of sessions sampled according to our [WikipediaPortal schema](https://meta.wikimedia.org/wiki/Schema:WikipediaPortal) at a 1:200 rate. This dashboard also tracks the proportion of events/visits/sessions to the portal that end in a click through to one of our projects - via search or via one of the links. This is expressed as a percentage - so a value of "39.4" means that 39.4% of visits end in a clickthrough.

Notes
------

* This dashboard is aim to expand the "other" countries display in the current [Portal Dashboard](http://discovery.wmflabs.org/portal/#country_breakdown) to reflect more data that we're interested in seeing. See [T138107](https://phabricator.wikimedia.org/T138107) for more details.
* Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.
* On 28 June 2016 our Event Logging system started recording a finer view of U.S. traffic, breaking it down into 5 regions:
    - **Northeast Region**
        - New England Division: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island and Vermont
        - Middle Atlantic Division: New Jersey, New York and Pennsylvania
    - **Midwest Region**
        - East North Central Division: Illinois, Indiana, Michigan, Ohio and Wisconsin
        - West North Central Division: Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota and South Dakota
    - **South Region**
        - South Atlantic Division: Delaware, District of Columbia, Florida, Georgia, Maryland, North Carolina, South Carolina, Virginia and West Virginia
        - East South Central Division: Alabama, Kentucky, Mississippi and Tennessee
        - West South Central Division: Arkansas, Louisiana, Oklahoma and Texas
    - **West Region**
        - Mountain Division: Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah and Wyoming
    - **Pacific Region**
        - Alaska, California, Hawaii, Oregon and Washington
    - **Other**
        - American Samoa, Guam, Northern Mariana Islands, Puerto Rico and Virgin Islands, U.S.
* The overall clickthrough rate presented here is the proportion of click events over the total number of landing events. The clickthrough rate per visit is the proportion of landing events that end in at least one click through. The clickthrough rate per session is the proportion of sessions that end in at least one click through. By design, a single session is at least 15 minutes but can last indefinitely. **For example**: a single session can last for hours if the "user" (e.g. a computer in a public library) keeps returning to the page before the 15 minute expiration time, thus resetting the timer; and if that single session has 1000 page visits and 500 clicks, then all 1500 of those events will be used in the calculation of the overall clickthrough rate. 

Outages and inaccuracies
------

- 13 September 2016: Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery-experimental.wmflabs.org/poultry/#all_country">
    http://discovery-experimental.wmflabs.org/poultry/#all_country
  </a>
</p>

