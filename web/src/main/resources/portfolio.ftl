<html lang="en">
<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <title>${pageTitle}</title>
    <script src="/plotly-1.49.5.js"></script>
    <script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
</script>
<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
</head>
<body>
<p>
	<#list overviews as ov>
    Name: ${ov.symbol} <br />
    Standard deviation: ${ov.standardDeviation}<br />
    Variance: ${ov.variance}<br />
	</#list>
</p>
 </body>
</html>


