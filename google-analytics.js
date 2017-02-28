// GTM:
(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
new Date().getTime(),event:'gtm.js'});
var f=d.getElementsByTagName(s)[0],j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';
j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;
f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer','GTM-PJ54BTC');

// Stock GA js (as of 2/20/2017):
(function(i,s,o,g,r,a,m){
  i['GoogleAnalyticsObject']=r;
  i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o),m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;
  m.parentNode.insertBefore(a,m)})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-92302344-1', 'auto');

// Sets appName so it doesn't need to be passed in the UI on screenview calls:
ga('set', 'appName', 'USFactors');

// Loads autotrack.js plugins:
ga('require', 'eventTracker');
ga('require', 'maxScrollTracker', {
  sessionTimeout: 30,
});
ga('require', 'outboundLinkTracker');
ga('require', 'pageVisibilityTracker', {
  sessionTimeout: 30,
});
ga('require', 'urlChangeTracker');

//// Updates the tracker to use `navigator.sendBeacon` if available.
ga('set', 'transport', 'beacon');

// End script by sending GA a pageview:
ga('send', 'pageview');
