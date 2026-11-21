// The light/dark switch in the nav: it flips the page's theme and remembers
// the choice (the script in layout.html applies it before the page paints).
(function () {
  var root = document.documentElement;
  var theme = document.querySelector(".theme");
  if (theme) {
    theme.addEventListener("click", function () {
      var dark = root.dataset.theme ? root.dataset.theme === "dark" : matchMedia("(prefers-color-scheme: dark)").matches;
      root.dataset.theme = dark ? "light" : "dark";
      try { localStorage.setItem("theme", root.dataset.theme); } catch (e) {}
    });
  }
})();
