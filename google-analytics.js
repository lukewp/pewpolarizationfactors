// Stock GA js (as of 2/20/2017):
(function(i,s,o,g,r,a,m){
  i['GoogleAnalyticsObject']=r;
  i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o),m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;
  m.parentNode.insertBefore(a,m)})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-92302344-1', 'auto');
ga('send', 'pageview');

// Sets appName so it doesn't need to be passed in the UI on screenview calls:
ga('set', 'appName', 'USFactors');

// Loads autotrack.js plugins:
ga('require', 'eventTracker');
ga('require', 'outboundLinkTracker');
ga('require', 'urlChangeTracker');

//// Updates the tracker to use `navigator.sendBeacon` if available.
//ga('set', 'transport', 'beacon');
