/* ----------------------------------------------------------------------------
   roperators — "I'm trying to…" task finder + small site polish.
   Dependency-free. Mounts into #rop-finder on the home page, carries its own
   styles (cache-proof), and also tidies the reference index.
   ------------------------------------------------------------------------- */
(function () {
  "use strict";

  var CATS = [
    "Strings", "Numbers", "Comparing", "Missing data",
    "Converting", "Files & system", "Flow & helpers"
  ];

  // The default (unfiltered) view shows the recipes marked p:1, with a
  // "show all" toggle — so the finder opens as a tidy shortlist, not a wall.
  var RECIPES = [
    // Strings
    {c:"Strings", p:1, t:"Join two strings together", k:"concatenate paste glue append plus add combine", code:'"foo" %+% "bar"', out:'"foobar"', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Remove part of a string", k:"subtract delete strip minus remove", code:'"abcabc" %-% "c"', out:'"abab"', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Repeat a string N times", k:"multiply repeat times duplicate", code:'"ha" %s*% 3', out:'"hahaha"', href:"reference/string_arithmetic.html"},
    {c:"Strings", t:"Count how often a pattern appears", k:"divide count occurrences how many", code:'"banana" %s/% "a"', out:'3', href:"reference/string_arithmetic.html"},
    {c:"Strings", p:1, t:"Drop variables into a string (f-string)", k:"interpolate template format glue sprintf paste fstring", code:'f("hi {name}, you are {age}")', href:"reference/f.html"},
    {c:"Strings", t:"Add text to a string variable in place", k:"append concatenate plus equals", code:'x %+=% "!"', href:"reference/assign_ops.html"},
    {c:"Strings", p:1, t:"Match ignoring case & spacing", k:"fuzzy approximate compare equal messy trim", code:'" Yes " %~% "yes"', out:'TRUE', href:"reference/fuzzy_match.html"},
    {c:"Strings", t:"SQL-style LIKE / case-insensitive match", k:"like grepl regex contains search pattern", code:'x %rlike% "foo"', href:"reference/pattern_matching.html"},
    {c:"Strings", t:"Case-sensitive Perl regex match", k:"perl regex grepl pattern case sensitive", code:'x %perl% "[A-Z]"', href:"reference/pattern_matching.html"},
    {c:"Strings", t:"Get the first / last / nth word", k:"split word extract token first last nth", code:'get_1st_word("Ada Lovelace")', out:'"Ada"', href:"reference/time_savers.html"},
    {c:"Strings", t:"Make an Oxford-comma list", k:"oxford comma series join list and conjunction", code:'paste_oxford("a", "b", "c")', out:'"a, b, and c"', href:"reference/paste_and_cat.html"},
    {c:"Strings", t:"Join a series with a conjunction", k:"series list join conjunction and or paste", code:'paste_series("a", "b", "c", conjunction = "or")', href:"reference/paste_and_cat.html"},

    // Numbers
    {c:"Numbers", p:1, t:"Add to / subtract from a variable in place", k:"increment plus minus equals modify reassign", code:'x %+=% 1', href:"reference/assign_ops.html"},
    {c:"Numbers", t:"Power or nth-root in place", k:"exponent power square root cube", code:'x %^=% 2   # or  x %root=% 3', href:"reference/assign_ops.html"},
    {c:"Numbers", p:1, t:"Divide without Inf/NaN on a zero", k:"safe divide division zero inf nan ratio", code:'10 %/0% 0', out:'NA', href:"reference/arithmetic_sugar.html"},
    {c:"Numbers", t:"n-choose-k / permutations", k:"choose permute combination factorial binomial", code:'5 %C% 3', out:'10', href:"reference/choose_permute.html"},
    {c:"Numbers", t:"Integrate a function inline", k:"integral integrate area calculus", code:'(function(x) x^2) %integrate% c(0, 1)', href:"reference/integrate.html"},
    {c:"Numbers", p:1, t:"Show a proportion as a percentage", k:"percent format proportion rate", code:'as.percent(2 / 3)', out:'"66.7%"', href:"reference/as.percent.html"},
    {c:"Numbers", t:"Make a ± tolerance interval", k:"tolerance plus minus margin interval bounds", code:'5 %+-% 0.5', out:'4.5 5.5', href:"reference/arithmetic_sugar.html"},
    {c:"Numbers", t:"Evenly spaced points around a value", k:"sequence seq around centre spacing points", code:'seq_around(10, n = 3)', out:'9.75 10 10.25', href:"reference/seq_around.html"},

    // Comparing
    {c:"Comparing", p:1, t:"Compare so NA == NA is TRUE", k:"equality missing na compare equal", code:'a %==% b', href:"reference/comparisons.html"},
    {c:"Comparing", p:1, t:"Floating-point equality that works", k:"float double precision 0.1 0.3 approximately equal round", code:'(0.1 + 0.1 + 0.1) %~=% 0.3', out:'TRUE', href:"reference/floating_point_comparisons.html"},
    {c:"Comparing", p:1, t:"Is a value between two bounds?", k:"between range within bounds interval", code:'5 %><% c(1, 10)', out:'TRUE', href:"reference/comparisons.html"},
    {c:"Comparing", t:"Strict equality (value AND class)", k:"identical strict type class triple equals", code:'x %===% 2L', href:"reference/comparisons.html"},
    {c:"Comparing", t:"Is x NOT in a set?", k:"not in notin membership exclude", code:'"z" %ni% c("a", "b")', out:'TRUE', href:"reference/logicals.html"},
    {c:"Comparing", t:"Exclusive-or / all-or-nothing", k:"xor aon and or both logical", code:'a %xor% b   # or  a %aon% b', href:"reference/logicals.html"},

    // Missing data
    {c:"Missing data", p:1, t:"Replace NAs with a value", k:"na missing fill replace impute default", code:'x %na<-% 0', href:"reference/overwrite_missing.html"},
    {c:"Missing data", p:1, t:"Mean / sum ignoring NAs", k:"na.rm missing mean sum complete cases ignore", code:'mean_cc(x)   # = mean(x, na.rm = TRUE)', href:"reference/complete_cases.html"},
    {c:"Missing data", t:"Is this safe to do maths on?", k:"na nan inf bad check guard validate calcs", code:'is.bad_for_calcs(x)', href:"reference/type_checks.html"},
    {c:"Missing data", t:"Count unique values, ignoring NA", k:"unique distinct count nunique", code:'n_unique(x, na.rm = TRUE)', href:"reference/n_unique.html"},

    // Converting
    {c:"Converting", p:1, t:"Shorthand as.integer / as.numeric / …", k:"convert coerce cast integer numeric character logical double", code:'int("42"); num("4.2"); chr(42); bool("T")', href:"reference/cleaner_conversions.html"},
    {c:"Converting", t:"Factor of numbers → real numbers", k:"factor numeric levels labels convert", code:'f.as.numeric(factor(c(11, 22, 33)))', out:'11 22 33', href:"reference/factor_conversion.html"},
    {c:"Converting", t:"Convert to a class chosen at runtime", k:"as class dynamic convert coerce", code:'as.class(255, "roman")', href:"reference/cleaner_conversions.html"},

    // Strings (regex)
    {c:"Strings", t:"Replace just the matched part (in place)", k:"regex gsub substitute replace pattern", code:'x %regex=% c("[0-9]+", "#")', href:"reference/overwrite_by_regex.html"},
    {c:"Strings", t:"Overwrite whole elements that match", k:"regex replace whole element grepl pattern", code:'x %regex<-% c("[0-9]+", "#")', href:"reference/assign_by_regex.html"},

    // Files & system
    {c:"Files & system", p:1, t:"Check a file's extension", k:"file csv txt excel extension type is", code:'is_csv_file("data.csv")', out:'TRUE', href:"reference/file_checks.html"},
    {c:"Files & system", t:"Which OS / R am I on?", k:"operating system mac windows linux os platform version", code:'get_os(); is.os_mac(); is.os_win()', href:"reference/os.html"},
    {c:"Files & system", t:"Read a TSV / pipe-delimited file", k:"read tsv psv tab pipe delimited table import", code:'read.tsv("data.tsv")', href:"reference/read.tsv.html"},

    // Flow & helpers
    {c:"Flow & helpers", p:1, t:"Fall back if an expression errors", k:"try catch error fallback default else safe", code:'risky_thing() %else% default', href:"reference/inline_fallback.html"},
    {c:"Flow & helpers", t:"Most frequent value(s)", k:"mode most common frequent majority", code:'get_most_frequent(c("a", "b", "b"))', out:'"b"', href:"reference/time_savers.html"},
    {c:"Flow & helpers", t:"Load a package, installing if needed", k:"library require install force load package", code:'library.force("somepkg")', href:"reference/library.force.html"}
  ];

  var DEFAULT_N = RECIPES.filter(function (r) { return r.p; }).length;

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
      ".rop-finder__input{width:100%;font-size:1.05rem;padding:.7rem 1rem;border:1.5px solid rgba(63,125,88,.45);border-radius:.55rem;background:#fffdf9;color:#33312e}",
      ".rop-finder__input:focus{outline:none;border-color:#3f7d58;box-shadow:0 0 0 .2rem rgba(63,125,88,.25)}",
      ".rop-finder__chips{display:flex;flex-wrap:wrap;gap:.45rem;margin:.9rem 0 .3rem}",
      ".rop-finder__chip{font-size:.85rem;padding:.4rem .85rem;min-height:40px;display:inline-flex;align-items:center;border:1px solid rgba(63,125,88,.55);border-radius:999px;background:transparent;color:#2f6b48;cursor:pointer}",
      ".rop-finder__chip:hover{background:rgba(63,125,88,.12)}",
      ".rop-finder__chip.is-active{background:#3f7d58;color:#fff;border-color:#3f7d58}",
      ".rop-finder__chip:focus-visible,.rop-card:focus-visible{outline:2px solid #2f6b48;outline-offset:2px}",
      ".rop-finder__count{font-size:.82rem;color:rgba(51,49,46,.78);margin:.45rem 0 .85rem}",
      ".rop-finder__showall{font:inherit;font-size:.82rem;color:#2f6b48;background:none;border:none;padding:0;cursor:pointer;text-decoration:underline;text-underline-offset:2px}",
      ".rop-finder__grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(212px,1fr));gap:.7rem}",
      ".rop-card{display:flex;flex-direction:column;gap:.35rem;min-height:128px;padding:.8rem .9rem;background:#fffdf9;border:1px solid rgba(63,125,88,.4);border-radius:.55rem;color:#33312e;transition:transform .1s ease,box-shadow .1s ease,border-color .1s ease}",
      ".rop-card,.rop-card:hover{text-decoration:none}",
      ".rop-card:hover,.rop-card:focus-visible{transform:translateY(-2px);border-color:#3f7d58;box-shadow:0 4px 14px rgba(63,125,88,.16)}",
      ".rop-card__cat{font-size:.66rem;text-transform:uppercase;letter-spacing:.06em;color:#2f6b48;font-weight:600}",
      ".rop-card__title{font-weight:600;line-height:1.25}",
      ".rop-card__code{display:block;font-size:.83rem;background:rgba(63,125,88,.08);padding:.3rem .45rem;border-radius:.35rem;overflow-x:auto;white-space:pre}",
      ".rop-card__out{font-size:.8rem;color:rgba(51,49,46,.78)}",
      ".rop-card__more{margin-top:auto;font-size:.78rem;color:#2f6b48;font-weight:600}",
      ".rop-finder__empty{color:rgba(51,49,46,.78);padding:.5rem 0}",
      "@media (max-width:520px){.rop-finder__grid{grid-template-columns:1fr}}"
    ].join("\n");
    var s = document.createElement("style");
    s.id = "rop-finder-style";
    s.textContent = css;
    document.head.appendChild(s);
  }

  function card(r) {
    var a = document.createElement("a");
    a.className = "rop-card";
    a.href = r.href;
    a.innerHTML =
      '<span class="rop-card__cat">' + escapeHtml(r.c) + "</span>" +
      '<span class="rop-card__title">' + escapeHtml(r.t) + "</span>" +
      '<code class="rop-card__code">' + escapeHtml(r.code) + "</code>" +
      '<span class="rop-card__out">' + (r.out ? "&rarr; " + escapeHtml(r.out) : "&nbsp;") + "</span>" +
      '<span class="rop-card__more">docs &rarr;</span>';
    return a;
  }

  function buildFinder(mount) {
    injectStyles();
    mount.innerHTML = "";
    mount.classList.add("rop-finder");
    mount.setAttribute("role", "search");
    mount.setAttribute("aria-label", "Find the right tool");

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

    var allCats = ["All"].concat(CATS);
    var active = "All";
    var showAll = false;
    var chipEls = {};
    var chips = document.createElement("div");
    chips.className = "rop-finder__chips";
    chips.setAttribute("role", "group");
    chips.setAttribute("aria-label", "Filter by category");
    allCats.forEach(function (cat) {
      var b = document.createElement("button");
      b.type = "button";
      b.className = "rop-finder__chip" + (cat === "All" ? " is-active" : "");
      b.textContent = cat;
      b.setAttribute("aria-pressed", cat === "All" ? "true" : "false");
      b.addEventListener("click", function () { active = cat; showAll = false; updateChips(); render(); });
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

    function matches(r, terms) {
      if (active !== "All" && r.c !== active) { return false; }
      if (!terms.length) { return true; }
      var hay = (r.t + " " + r.k + " " + r.code + " " + r.c).toLowerCase();
      return terms.every(function (term) { return hay.indexOf(term) >= 0; });
    }

    function render() {
      var terms = input.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
      var filtering = terms.length > 0 || active !== "All";
      var pool = RECIPES.filter(function (r) { return matches(r, terms); });
      var shown = pool;
      var trimmed = false;
      if (!filtering && !showAll) {
        var pop = pool.filter(function (r) { return r.p; });
        if (pop.length) { shown = pop; trimmed = pool.length > pop.length; }
      }

      grid.innerHTML = "";
      shown.forEach(function (r) { grid.appendChild(card(r)); });

      count.innerHTML = "";
      if (shown.length === 0) {
        grid.innerHTML =
          '<p class="rop-finder__empty">No match — try another word, or browse the ' +
          '<a href="reference/index.html">full reference</a>.</p>';
        count.textContent = "0 of " + RECIPES.length + " tools";
        return;
      }
      if (trimmed) {
        count.appendChild(document.createTextNode(
          "Showing " + shown.length + " popular tools — type to filter, or "));
        var btn = document.createElement("button");
        btn.type = "button";
        btn.className = "rop-finder__showall";
        btn.textContent = "show all " + RECIPES.length;
        btn.addEventListener("click", function () { showAll = true; render(); });
        count.appendChild(btn);
      } else {
        count.textContent = shown.length + " of " + RECIPES.length +
          (shown.length === 1 ? " tool" : " tools");
      }
    }

    var timer;
    input.addEventListener("input", function () {
      clearTimeout(timer);
      timer = setTimeout(render, 70);
    });

    render();
  }

  // Cosmetic: pkgdown renders operator topics in the reference index with
  // literal backticks (e.g. `%+%`). Strip them so the index reads cleanly.
  function tidyReferenceIndex() {
    var idx = document.querySelector(".template-reference-index");
    if (!idx) { return; }
    var links = idx.querySelectorAll("dt code a, dt a, dt code");
    Array.prototype.forEach.call(links, function (el) {
      if (el.children.length === 0 && /^`.*`$/.test(el.textContent.trim())) {
        el.textContent = el.textContent.trim().replace(/^`+|`+$/g, "");
      }
    });
  }

  // De-yellow the rendered code blocks site-wide. Injected from JS (not only the
  // compiled stylesheet) so it survives a stale, cache-pinned bootstrap.css.
  function injectCodeStyles() {
    if (document.getElementById("rop-code-style")) { return; }
    var css = [
      "div.sourceCode,pre.sourceCode,pre.downlit,pre{background:#fffdf9 !important;color:#33312e}",
      ".sourceCode code,pre code,code.sourceCode{background:transparent !important}",
      ".sourceCode span{background:transparent !important}",
      ".sourceCode .st{color:#2f6b48}",
      ".sourceCode .co{color:#8a857c;font-style:italic}",
      ".sourceCode .fu{color:#33312e;font-weight:600}",
      ".sourceCode .kw{color:#9c4221;font-weight:600}",
      ".sourceCode .cn,.sourceCode .fl,.sourceCode .dv{color:#9c4221}",
      ".sourceCode .va{color:#33312e}",
      ".sourceCode .op{color:#2f6b48;font-weight:600}",
      ".sourceCode .op a,.sourceCode .op a:hover{color:#2f6b48;text-decoration:none}"
    ].join("\n");
    var s = document.createElement("style");
    s.id = "rop-code-style";
    s.textContent = css;
    document.head.appendChild(s);
  }

  /* ---- "Taste Test" hero --------------------------------------------------
     base-R vs roperators duel; you pick, it reveals the real base-R gotcha and
     celebrates (or gently roasts) your taste. Self-injects its styles. */
  var PAIRS = [
    { task: "Drop values into a sentence",
      base: 'paste0("Hi ", name, ", you have ", n, " messages")', baseOut: '"Hi Ben, you have 2 messages"',
      rop:  'f("Hi {name}, you have {n} messages")', ropOut: '"Hi Ben, you have 2 messages"',
      reveal: "f-strings let you read the sentence, not the plumbing of quotes and commas." },
    { task: "Compare two vectors that contain NAs",
      base: 'c(1, NA, 3) == c(1, NA, 4)', baseOut: "TRUE   NA  FALSE",
      rop:  'c(1, NA, 3) %==% c(1, NA, 4)', ropOut: "TRUE TRUE FALSE",
      reveal: "%==% treats NA == NA as TRUE, so the NA doesn’t leak through — you get the answer you meant." },
    { task: "Check whether two decimals are equal",
      base: 'isTRUE(all.equal(0.1 + 0.1 + 0.1, 0.3))', baseOut: "TRUE   # and plain == is FALSE!",
      rop:  '(0.1 + 0.1 + 0.1) %~=% 0.3', ropOut: "TRUE",
      reveal: "%~=% reads like the maths you meant — no all.equal() ceremony, no floating-point surprise." },
    { task: "Fill the NAs with zero, in place",
      base: 'x[is.na(x)] <- 0', baseOut: "",
      rop:  'x %na<-% 0', ropOut: "",
      reveal: "%na<-% says exactly what you mean — no indexing riddle to decode." },
    { task: "Divide, but a stray zero shouldn’t blow up",
      base: 'ifelse(b == 0, NA, a / b)', baseOut: "",
      rop:  'a %/0% b', ropOut: "",
      reveal: "%/0% bakes in the guard: a zero divisor yields NA instead of Inf/NaN poisoning a whole mean." }
  ];

  function highlightOps(escaped) {
    return escaped.replace(/%[^%]*%/g, function (m) { return '<span class="rop-tk-op">' + m + "</span>"; });
  }
  function escapeAttr(s) {
    return String(s).replace(/&/g, "&amp;").replace(/"/g, "&quot;");
  }

  function injectHeroStyles() {
    if (document.getElementById("rop-hero-style")) { return; }
    var css = [
      ".rop-hero{margin:1.4rem 0 2rem;padding:1.5rem;background:#fffdf9;border:1px solid rgba(63,125,88,.22);border-radius:.85rem;box-shadow:0 4px 20px rgba(51,49,46,.06)}",
      ".rop-hero__eyebrow{font-size:.72rem;text-transform:uppercase;letter-spacing:.08em;color:#2f6b48;font-weight:600}",
      ".rop-hero__prompt{font-family:Fraunces,Georgia,serif;font-size:1.7rem;font-weight:600;margin:.2rem 0 .3rem;border:0;padding:0}",
      ".contents .rop-hero__prompt::after{content:none;display:none}",
      ".rop-hero__task{color:#6b6760;margin:0 0 1rem;font-size:.95rem}",
      ".rop-hero__tasknum{display:inline-block;background:rgba(63,125,88,.1);color:#2f6b48;border-radius:999px;padding:.05rem .5rem;font-size:.78rem;font-weight:600;margin-right:.25rem}",
      ".rop-hero__duel{display:grid;grid-template-columns:1fr auto 1fr;align-items:stretch;gap:.7rem}",
      ".rop-hero__vs{align-self:center;font-family:Fraunces,serif;color:#2f6b48;font-weight:600;border:1px solid rgba(63,125,88,.4);border-radius:999px;width:2.1rem;height:2.1rem;display:flex;align-items:center;justify-content:center;font-size:.85rem}",
      ".rop-hero__card{position:relative;text-align:left;display:flex;align-items:center;min-height:62px;padding:.9rem 1rem;background:#fbf7ef;border:1px solid rgba(63,125,88,.22);border-radius:.6rem;cursor:pointer;transition:transform .1s ease,border-color .1s ease,box-shadow .1s ease}",
      ".rop-hero__card:hover{transform:translateY(-2px);border-color:#3f7d58;box-shadow:0 4px 14px rgba(63,125,88,.16)}",
      ".rop-hero__card:focus-visible{outline:2px solid #2f6b48;outline-offset:2px}",
      ".rop-hero__code{font-family:'JetBrains Mono',ui-monospace,monospace;font-size:.92rem;color:#33312e;background:transparent;white-space:pre-wrap;word-break:break-word;padding:0}",
      ".rop-tk-op{color:#2f6b48;font-weight:600}",
      ".rop-hero__badge{position:absolute;top:-.6rem;right:.6rem;font-size:.62rem;text-transform:uppercase;letter-spacing:.05em;font-weight:700;padding:.1rem .45rem;border-radius:999px}",
      ".rop-hero__card.is-rop .rop-hero__badge{background:#2f6b48;color:#fff}",
      ".rop-hero__card.is-base .rop-hero__badge{background:#e7e2d8;color:#6b6760}",
      ".rop-hero__card.is-locked{cursor:default}",
      ".rop-hero__card.is-dimmed{opacity:.62}",
      ".rop-hero__card.is-chosen-win{border-color:#3f7d58;box-shadow:0 0 0 2.5px #3f7d58}",
      ".rop-hero__card.is-chosen-roast{border-color:#caa15a;box-shadow:0 0 0 2.5px #caa15a}",
      ".rop-hero__card.is-answer{background:rgba(63,125,88,.08);border-color:#3f7d58}",
      ".rop-hero__result{min-height:3.4rem;margin-top:1rem}",
      ".rop-hero__banner{border-radius:.5rem;padding:.6rem .8rem;margin-bottom:.5rem}",
      ".rop-hero__banner.is-win{background:rgba(63,125,88,.1);border-left:3px solid #3f7d58}",
      ".rop-hero__banner.is-roast{background:#f6e7cf;border-left:3px solid #caa15a;color:#5c4514}",
      ".rop-hero__banner code{background:rgba(0,0,0,.06);color:inherit;padding:.05rem .3rem;border-radius:.3rem;font-family:'JetBrains Mono',ui-monospace,monospace}",
      ".rop-hero__answer{font-size:.92rem;color:#33312e;display:flex;align-items:center;flex-wrap:wrap;gap:.4rem;margin-top:.2rem}",
      ".rop-hero__copy{font:inherit;font-size:.74rem;color:#2f6b48;background:rgba(63,125,88,.1);border:1px solid rgba(63,125,88,.3);border-radius:.35rem;padding:.05rem .45rem;cursor:pointer}",
      ".rop-hero__foot{display:flex;align-items:center;gap:.6rem;margin-top:1rem}",
      ".rop-hero__dots{display:flex;gap:.3rem;flex:1}",
      ".rop-hero__dot{width:.5rem;height:.5rem;border-radius:999px;background:rgba(63,125,88,.25)}",
      ".rop-hero__dot.is-on{background:#3f7d58}",
      ".rop-hero__next{font:inherit;font-weight:600;color:#fff;background:#3f7d58;border:none;border-radius:.5rem;padding:.45rem .9rem;cursor:pointer}",
      ".rop-hero__next:focus-visible,.rop-hero__cta:focus-visible{outline:2px solid #2f6b48;outline-offset:2px}",
      ".rop-hero__tally{font-size:.8rem;color:#6b6760;font-weight:600}",
      ".rop-hero__score{font-size:1.05rem;margin:.2rem 0 .4rem}",
      ".rop-hero__ctas{display:flex;flex-wrap:wrap;gap:.6rem;margin:1rem 0 .6rem}",
      ".rop-hero__cta{display:inline-block;font-weight:600;text-decoration:none;border-radius:.5rem;padding:.5rem .9rem;border:1px solid #3f7d58;color:#2f6b48}",
      ".rop-hero__cta--primary{background:#3f7d58;color:#fff}",
      ".rop-hero__again{font:inherit;font-size:.8rem;color:#6b6760;background:none;border:none;text-decoration:underline;cursor:pointer;padding:0}",
      "@media (max-width:620px){.rop-hero__duel{grid-template-columns:1fr}.rop-hero__vs{justify-self:center;margin:.1rem 0}.rop-hero__ctas .rop-hero__cta{flex:1;text-align:center}}",
      "@media (prefers-reduced-motion:reduce){.rop-hero__card,.rop-hero__card:hover{transition:none;transform:none}}"
    ].join("\n");
    var s = document.createElement("style");
    s.id = "rop-hero-style";
    s.textContent = css;
    document.head.appendChild(s);
  }

  function copyText(text, btn) {
    var done = function () { btn.textContent = "Copied!"; setTimeout(function () { btn.textContent = "Copy"; }, 1400); };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(done, function () {});
    } else {
      var ta = document.createElement("textarea");
      ta.value = text; ta.style.position = "fixed"; ta.style.opacity = "0";
      document.body.appendChild(ta); ta.select();
      try { document.execCommand("copy"); done(); } catch (e) {}
      document.body.removeChild(ta);
    }
  }

  function buildHero(mount) {
    injectHeroStyles();
    mount.classList.add("rop-hero");
    mount.setAttribute("role", "group");
    mount.setAttribute("aria-label", "Which would you rather write?");

    var idx = 0, wins = 0, seen = 0, answered = false;
    try {
      var ss = window.sessionStorage;
      wins = parseInt(ss.getItem("rop-taste-wins"), 10) || 0;
      seen = parseInt(ss.getItem("rop-taste-seen"), 10) || 0;
    } catch (e) {}
    function save() { try { var s2 = window.sessionStorage; s2.setItem("rop-taste-wins", wins); s2.setItem("rop-taste-seen", seen); } catch (e) {} }
    function tallyText() { return seen ? "Good taste: " + wins + "/" + seen : ""; }

    function heroCard(slot, item) {
      return '<button type="button" class="rop-hero__card" aria-pressed="false" data-rop="' + (item.rop ? 1 : 0) +
        '" aria-label="Option ' + slot + ": " + escapeAttr(item.code) + '">' +
        '<span class="rop-hero__badge" aria-hidden="true"></span>' +
        '<code class="rop-hero__code">' + highlightOps(escapeHtml(item.code)) + "</code></button>";
    }

    function renderRound() {
      answered = false;
      var p = PAIRS[idx];
      var ropLeft = Math.random() < 0.5;
      var left = ropLeft ? { code: p.rop, rop: true } : { code: p.base, rop: false };
      var right = ropLeft ? { code: p.base, rop: false } : { code: p.rop, rop: true };
      var dots = "";
      for (var i = 0; i < PAIRS.length; i++) { dots += '<span class="rop-hero__dot' + (i === idx ? " is-on" : "") + '"></span>'; }
      mount.innerHTML =
        '<div class="rop-hero__eyebrow">roperators in one decision</div>' +
        '<h2 class="rop-hero__prompt" id="rop-hero-prompt">Which would you rather write?</h2>' +
        '<p class="rop-hero__task"><span class="rop-hero__tasknum">' + (idx + 1) + "/" + PAIRS.length + "</span> " +
        escapeHtml(p.task) + " — tap the one you’d rather write.</p>" +
        '<div class="rop-hero__duel">' + heroCard("A", left) +
        '<div class="rop-hero__vs" aria-hidden="true">vs</div>' + heroCard("B", right) + "</div>" +
        '<div class="rop-hero__result" role="status" aria-live="polite"></div>' +
        '<div class="rop-hero__foot"><span class="rop-hero__dots">' + dots + "</span>" +
        '<button type="button" class="rop-hero__next" hidden>' +
        (idx < PAIRS.length - 1 ? "Next →" : "See your taste →") + "</button>" +
        '<span class="rop-hero__tally" aria-live="polite">' + tallyText() + "</span></div>";
      var cards = mount.querySelectorAll(".rop-hero__card");
      Array.prototype.forEach.call(cards, function (c) {
        c.addEventListener("click", function () { choose(c, cards, p); });
      });
    }

    function choose(card, cards, p) {
      if (answered) { return; }
      answered = true;
      var pickedRop = card.getAttribute("data-rop") === "1";
      if (pickedRop) { wins += 1; }
      seen += 1; save();
      Array.prototype.forEach.call(cards, function (c) {
        var isRop = c.getAttribute("data-rop") === "1";
        c.classList.add("is-locked", isRop ? "is-rop" : "is-base");
        c.setAttribute("aria-pressed", c === card ? "true" : "false");
        c.querySelector(".rop-hero__badge").textContent = isRop ? "roperators" : "base R";
        if (isRop) { c.classList.add("is-answer"); }
        if (c !== card) { c.classList.add("is-dimmed"); }
      });
      card.classList.add(pickedRop ? "is-chosen-win" : "is-chosen-roast");

      var banner = pickedRop
        ? '<div class="rop-hero__banner is-win"><strong>Good taste.</strong> ' + escapeHtml(p.reveal) + "</div>"
        : '<div class="rop-hero__banner is-roast"><strong>Bold choice — R may quietly betray that one.</strong> ' +
          (p.baseOut ? "base R returns <code>" + escapeHtml(p.baseOut) + "</code>. " : "") + escapeHtml(p.reveal) + "</div>";
      var answer = '<div class="rop-hero__answer">the calmer way&nbsp; <code class="rop-hero__code">' +
        highlightOps(escapeHtml(p.rop)) + '</code> <button type="button" class="rop-hero__copy">Copy</button></div>';
      var res = mount.querySelector(".rop-hero__result");
      res.innerHTML = banner + answer;
      var copyBtn = res.querySelector(".rop-hero__copy");
      if (copyBtn) { copyBtn.addEventListener("click", function () { copyText(p.rop, copyBtn); }); }

      var nb = mount.querySelector(".rop-hero__next");
      nb.hidden = false;
      nb.addEventListener("click", nextRound);
      nb.focus();
      var t = mount.querySelector(".rop-hero__tally");
      if (t) { t.textContent = tallyText(); }
    }

    function nextRound() {
      idx += 1;
      if (idx >= PAIRS.length) { renderScorecard(); } else { renderRound(); }
    }

    function renderScorecard() {
      var good = wins >= Math.ceil(PAIRS.length / 2);
      var headline = good ? "You’ve got good roperators taste." : "A base-R purist — we can work with that.";
      mount.innerHTML =
        '<div class="rop-hero__eyebrow">verdict</div>' +
        '<h2 class="rop-hero__prompt">' + headline + "</h2>" +
        '<p class="rop-hero__score">You preferred the roperator <strong>' + wins + " of " + seen + "</strong> times.</p>" +
        '<p class="rop-hero__task">Every roperator turns a fiddly base-R line into one that reads like the thought ' +
        "behind it — dozens of them, all base R underneath, zero dependencies.</p>" +
        '<div class="rop-hero__ctas">' +
        '<a class="rop-hero__cta rop-hero__cta--primary" href="#installation">Install it →</a>' +
        '<a class="rop-hero__cta" href="reference/index.html">Browse all the tools →</a></div>' +
        '<button type="button" class="rop-hero__again">Play again</button>';
      var again = mount.querySelector(".rop-hero__again");
      again.addEventListener("click", function () { idx = 0; renderRound(); });
    }

    renderRound();
  }

  ready(function () {
    injectCodeStyles();
    var finder = document.getElementById("rop-finder");
    if (finder) { buildFinder(finder); }
    var hero = document.getElementById("rop-hero");
    if (hero) { buildHero(hero); }
    tidyReferenceIndex();
  });
})();
