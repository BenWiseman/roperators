/* ----------------------------------------------------------------------------
   roperators — "I'm trying to…" task finder
   A dependency-free widget that mounts into #rop-finder on the home page and
   lets people search by intent (join strings, round, missing values, …) rather
   than by remembering operator names. Degrades to a plain link on GitHub.
   ------------------------------------------------------------------------- */
(function () {
  "use strict";

  var CATS = [
    "Strings", "Numbers", "Comparing", "Missing data",
    "Converting", "Files & system", "Flow & helpers"
  ];

  // task phrasings -> the tool that does it. `out` is optional (shown as result).
  var RECIPES = [
    // Strings
    {c:"Strings", t:"Join two strings together", k:"concatenate paste glue append plus add combine", code:'"foo" %+% "bar"', out:'"foobar"', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Remove part of a string", k:"subtract delete strip minus remove", code:'"abcabc" %-% "c"', out:'"abab"', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Repeat a string N times", k:"multiply repeat times duplicate", code:'"ha" %s*% 3', out:'"hahaha"', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Count how often a pattern appears", k:"divide count occurrences how many", code:'"banana" %s/% "a"', out:'3', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Drop variables into a string (f-string)", k:"interpolate template format glue sprintf paste fstring", code:'f("hi {name}, you are {age}")', href:"reference/f.html"},
    {c:"Strings", t:"Add text to a string variable in place", k:"append concatenate plus equals", code:'x %+=% "!"', href:"reference/assign_ops.html"},
    {c:"Strings", t:"Match ignoring case & spacing", k:"fuzzy approximate compare equal messy trim", code:'" Yes " %~% "yes"', out:'TRUE', href:"reference/fuzzy_match.html"},
    {c:"Strings", t:"SQL-style LIKE / case-insensitive match", k:"like grepl regex contains search pattern", code:'x %rlike% "foo"', href:"reference/pattern_matching.html"},
    {c:"Strings", t:"Get the first / last / nth word", k:"split word extract token first last nth", code:'get_1st_word("Ada Lovelace")', out:'"Ada"', href:"reference/time_savers.html"},
    {c:"Strings", t:"Make an Oxford-comma list", k:"oxford comma series join list and conjunction", code:'paste_oxford("a", "b", "c")', out:'"a, b, and c"', href:"reference/paste_and_cat.html"},

    // Numbers
    {c:"Numbers", t:"Add to / subtract from a variable in place", k:"increment plus minus equals modify reassign", code:'x %+=% 1', href:"reference/assign_ops.html"},
    {c:"Numbers", t:"Power or nth-root in place", k:"exponent power square root cube", code:'x %^=% 2   # or  x %root=% 3', href:"reference/assign_ops.html"},
    {c:"Numbers", t:"Divide without Inf/NaN on a zero", k:"safe divide division zero inf nan ratio", code:'10 %/0% 0', out:'NA', href:"reference/arithmetic_sugar.html"},
    {c:"Numbers", t:"n-choose-k / permutations", k:"choose permute combination factorial binomial", code:'5 %C% 3', out:'10', href:"reference/choose_permute.html"},
    {c:"Numbers", t:"Integrate a function inline", k:"integral integrate area calculus", code:'(function(x) x^2) %integrate% c(0, 1)', href:"reference/integrate.html"},
    {c:"Numbers", t:"Show a proportion as a percentage", k:"percent format proportion rate", code:'as.percent(2 / 3)', out:'"66.7%"', href:"reference/as.percent.html"},
    {c:"Numbers", t:"Make a ± tolerance interval", k:"tolerance plus minus margin interval bounds", code:'5 %+-% 0.5', out:'4.5 5.5', href:"reference/arithmetic_sugar.html"},

    // Comparing
    {c:"Comparing", t:"Compare so NA == NA is TRUE", k:"equality missing na compare equal", code:'a %==% b', href:"reference/comparisons.html"},
    {c:"Comparing", t:"Floating-point equality that works", k:"float double precision 0.1 0.3 approximately equal round", code:'(0.1 + 0.1 + 0.1) %~=% 0.3', out:'TRUE', href:"reference/floating_point_comparisons.html"},
    {c:"Comparing", t:"Is a value between two bounds?", k:"between range within bounds interval", code:'5 %><% c(1, 10)', out:'TRUE', href:"reference/comparisons.html"},
    {c:"Comparing", t:"Strict equality (value AND class)", k:"identical strict type class triple equals", code:'x %===% 2L', href:"reference/comparisons.html"},
    {c:"Comparing", t:"Is x NOT in a set?", k:"not in notin membership exclude", code:'"z" %ni% c("a", "b")', out:'TRUE', href:"reference/logicals.html"},
    {c:"Comparing", t:"Exclusive-or / all-or-nothing", k:"xor and or both logical", code:'a %xor% b', href:"reference/logicals.html"},

    // Missing data
    {c:"Missing data", t:"Replace NAs with a value", k:"na missing fill replace impute default", code:'x %na<-% 0', href:"reference/overwrite_missing.html"},
    {c:"Missing data", t:"Mean / sum ignoring NAs", k:"na.rm missing mean sum complete cases ignore", code:'mean_cc(x)   # = mean(x, na.rm = TRUE)', href:"reference/complete_cases.html"},
    {c:"Missing data", t:"Is this safe to do maths on?", k:"na nan inf bad check guard validate calcs", code:'is.bad_for_calcs(x)', href:"reference/type_checks.html"},
    {c:"Missing data", t:"Count unique values, ignoring NA", k:"unique distinct count nunique", code:'n_unique(x, na.rm = TRUE)', href:"reference/n_unique.html"},

    // Converting
    {c:"Converting", t:"Shorthand as.integer / as.numeric / …", k:"convert coerce cast integer numeric character logical double", code:'int("42"); num("4.2"); chr(42); bool("T")', href:"reference/cleaner_conversions.html"},
    {c:"Converting", t:"Factor of numbers → real numbers", k:"factor numeric levels labels convert", code:'f.as.numeric(factor(c(11, 22, 33)))', out:'11 22 33', href:"reference/factor_conversion.html"},
    {c:"Converting", t:"Convert to a class chosen at runtime", k:"as class dynamic convert coerce", code:'as.class(255, "roman")', href:"reference/cleaner_conversions.html"},

    // Patterns / regex (filed under Strings category chip via 'Strings')
    {c:"Strings", t:"Replace just the matched part (in place)", k:"regex gsub substitute replace pattern", code:'x %regex=% c("[0-9]+", "#")', href:"reference/overwrite_by_regex.html"},
    {c:"Strings", t:"Overwrite whole elements that match", k:"regex replace whole element grepl pattern", code:'x %regex<-% c("[0-9]+", "#")', href:"reference/assign_by_regex.html"},

    // Files & system
    {c:"Files & system", t:"Check a file's extension", k:"file csv txt excel extension type is", code:'is_csv_file("data.csv")', out:'TRUE', href:"reference/file_checks.html"},
    {c:"Files & system", t:"Which OS / R am I on?", k:"operating system mac windows linux os platform version", code:'get_os(); is.os_mac(); is.os_win()', href:"reference/os.html"},
    {c:"Files & system", t:"Read a TSV / pipe-delimited file", k:"read tsv psv tab pipe delimited table import", code:'read.tsv("data.tsv")', href:"reference/read.tsv.html"},

    // Flow & helpers
    {c:"Flow & helpers", t:"Fall back if an expression errors", k:"try catch error fallback default else safe", code:'risky_thing() %else% default', href:"reference/inline_fallback.html"},
    {c:"Flow & helpers", t:"Most frequent value(s)", k:"mode most common frequent majority", code:'get_most_frequent(c("a", "b", "b"))', out:'"b"', href:"reference/time_savers.html"},
    {c:"Flow & helpers", t:"Load a package, installing if needed", k:"library require install force load package", code:'library.force("somepkg")', href:"reference/library.force.html"}
  ];

  function ready(fn) {
    if (document.readyState !== "loading") { fn(); }
    else { document.addEventListener("DOMContentLoaded", fn); }
  }

  function escapeHtml(s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  // The widget carries its own styles so its look always arrives with its
  // markup — independent of the (cacheable, version-pinned) site stylesheet.
  function injectStyles() {
    if (document.getElementById("rop-finder-style")) { return; }
    var css = [
      ".rop-finder{margin:1.6rem 0 2.6rem;padding:1.4rem;background:linear-gradient(180deg,rgba(63,125,88,.08),rgba(63,125,88,.03));border:1px solid rgba(63,125,88,.22);border-radius:.85rem}",
      ".rop-finder__label{display:block;font-family:Fraunces,Georgia,serif;font-size:1.5rem;font-weight:600;margin-bottom:.55rem}",
      ".rop-finder__input{width:100%;font-size:1.05rem;padding:.7rem 1rem;border:1.5px solid rgba(63,125,88,.35);border-radius:.55rem;background:#fffdf9;color:#33312e}",
      ".rop-finder__input:focus{outline:none;border-color:#3f7d58;box-shadow:0 0 0 .2rem rgba(63,125,88,.18)}",
      ".rop-finder__chips{display:flex;flex-wrap:wrap;gap:.4rem;margin:.9rem 0 .3rem}",
      ".rop-finder__chip{font-size:.85rem;padding:.25rem .75rem;border:1px solid rgba(63,125,88,.3);border-radius:999px;background:transparent;color:#3f7d58;cursor:pointer}",
      ".rop-finder__chip:hover{background:rgba(63,125,88,.12)}",
      ".rop-finder__chip.is-active{background:#3f7d58;color:#fff;border-color:#3f7d58}",
      ".rop-finder__count{font-size:.8rem;color:rgba(51,49,46,.6);margin:.35rem 0 .8rem}",
      ".rop-finder__grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(232px,1fr));gap:.7rem}",
      ".rop-card{display:flex;flex-direction:column;gap:.35rem;padding:.8rem .9rem;background:#fffdf9;border:1px solid rgba(63,125,88,.18);border-radius:.55rem;color:#33312e;transition:transform .1s ease,box-shadow .1s ease,border-color .1s ease}",
      ".rop-card,.rop-card:hover{text-decoration:none}",
      ".rop-card:hover{transform:translateY(-2px);border-color:#3f7d58;box-shadow:0 4px 14px rgba(63,125,88,.16)}",
      ".rop-card__cat{font-size:.66rem;text-transform:uppercase;letter-spacing:.06em;color:#3f7d58;font-weight:600}",
      ".rop-card__title{font-weight:600;line-height:1.25}",
      ".rop-card__code{display:block;font-size:.83rem;background:rgba(63,125,88,.08);padding:.3rem .45rem;border-radius:.35rem;white-space:pre-wrap;word-break:break-word}",
      ".rop-card__out{font-size:.8rem;color:rgba(51,49,46,.6)}",
      ".rop-card__more{margin-top:auto;font-size:.78rem;color:#3f7d58;font-weight:600}",
      ".rop-finder__empty{color:rgba(51,49,46,.6);padding:.5rem 0}"
    ].join("\n");
    var s = document.createElement("style");
    s.id = "rop-finder-style";
    s.textContent = css;
    document.head.appendChild(s);
  }

  ready(function () {
    var mount = document.getElementById("rop-finder");
    if (!mount) { return; }            // only on the home page
    injectStyles();
    mount.innerHTML = "";
    mount.classList.add("rop-finder");

    // --- search box ---------------------------------------------------------
    var head = document.createElement("div");
    head.className = "rop-finder__head";
    var label = document.createElement("label");
    label.className = "rop-finder__label";
    label.setAttribute("for", "rop-finder-input");
    label.textContent = "I’m trying to…";
    var input = document.createElement("input");
    input.type = "search";
    input.id = "rop-finder-input";
    input.className = "rop-finder__input";
    input.placeholder = "join two strings, round, missing values, read a file…";
    input.setAttribute("autocomplete", "off");
    input.setAttribute("aria-controls", "rop-finder-grid");
    head.appendChild(label);
    head.appendChild(input);
    mount.appendChild(head);

    // --- category chips -----------------------------------------------------
    var allCats = ["All"].concat(CATS);
    var active = "All";
    var chipEls = {};
    var chips = document.createElement("div");
    chips.className = "rop-finder__chips";
    allCats.forEach(function (cat) {
      var b = document.createElement("button");
      b.type = "button";
      b.className = "rop-finder__chip" + (cat === "All" ? " is-active" : "");
      b.textContent = cat;
      b.setAttribute("aria-pressed", cat === "All" ? "true" : "false");
      b.addEventListener("click", function () { active = cat; updateChips(); render(); });
      chips.appendChild(b);
      chipEls[cat] = b;
    });
    mount.appendChild(chips);

    function updateChips() {
      allCats.forEach(function (cat) {
        var on = cat === active;
        chipEls[cat].classList.toggle("is-active", on);
        chipEls[cat].setAttribute("aria-pressed", on ? "true" : "false");
      });
    }

    var count = document.createElement("p");
    count.className = "rop-finder__count";
    count.setAttribute("aria-live", "polite");
    mount.appendChild(count);

    var grid = document.createElement("div");
    grid.className = "rop-finder__grid";
    grid.id = "rop-finder-grid";
    mount.appendChild(grid);

    // --- matching & rendering ----------------------------------------------
    function matches(r, terms) {
      if (active !== "All" && r.c !== active) { return false; }
      if (!terms.length) { return true; }
      var hay = (r.t + " " + r.k + " " + r.code + " " + r.c).toLowerCase();
      return terms.every(function (term) { return hay.indexOf(term) >= 0; });
    }

    function card(r) {
      var a = document.createElement("a");
      a.className = "rop-card";
      a.href = r.href;
      a.innerHTML =
        '<span class="rop-card__cat">' + escapeHtml(r.c) + "</span>" +
        '<span class="rop-card__title">' + escapeHtml(r.t) + "</span>" +
        '<code class="rop-card__code">' + escapeHtml(r.code) + "</code>" +
        (r.out ? '<span class="rop-card__out">&rarr; ' + escapeHtml(r.out) + "</span>" : "") +
        '<span class="rop-card__more">docs &rarr;</span>';
      return a;
    }

    function render() {
      var terms = input.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
      grid.innerHTML = "";
      var shown = 0;
      RECIPES.forEach(function (r) {
        if (matches(r, terms)) { grid.appendChild(card(r)); shown += 1; }
      });
      if (shown === 0) {
        var empty = document.createElement("p");
        empty.className = "rop-finder__empty";
        empty.innerHTML = "No match — try another word, or browse the " +
          '<a href="reference/index.html">full reference</a>.';
        grid.appendChild(empty);
      }
      count.textContent = shown + (shown === 1 ? " recipe" : " recipes");
    }

    var timer;
    input.addEventListener("input", function () {
      clearTimeout(timer);
      timer = setTimeout(render, 70);
    });

    render();
  });
})();
