<script>
(function() {
  var divHTML = document.querySelectorAll(".details-open-code");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[0];
    outputNode.outerHTML = "<details open class='output'><summary>código</summary>" + outputNode.outerHTML + "]</details>";
  })
})();
(function() {
  var divHTML = document.querySelectorAll(".details-code");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[0];
    outputNode.outerHTML = "<details class='output'><summary>código</summary>" + outputNode.outerHTML + "]</details>";
  })
})();
(function() {
  var divHTML = document.querySelectorAll(".details-open");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[1];
    outputNode.outerHTML = "<details open class='output'><summary>salida</summary>" + outputNode.outerHTML + "]</details>";
  })
})();
(function() {
  var divHTML = document.querySelectorAll(".details");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[1];
    outputNode.outerHTML = "<details class='output'><summary>salida</summary>" + outputNode.outerHTML + "]</details>";
  })
})();
</script>