(function () {
  var KEY = "win95";

  function set(on) {
    document.body.classList.toggle("win95", on);
    try {
      localStorage.setItem(KEY, on ? "1" : "0");
    } catch (e) {}
  }

  var toggleBtn = document.getElementById("win95-toggle");
  if (toggleBtn) {
    toggleBtn.addEventListener("click", function () {
      set(!document.body.classList.contains("win95"));
    });
  }

  var closeBtn = document.getElementById("win95-close");
  if (closeBtn) {
    closeBtn.addEventListener("click", function () {
      set(false);
    });
  }
})();
