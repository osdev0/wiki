/* Copyright (c) 2021 Arsen Arsenovic, CC0 */
function blowSearchUp(searchDiv) {
    searchDiv.classList.add("stork-fullscreen");
}

function hideSearch(searchDiv) {
    searchDiv.classList.remove("stork-fullscreen");
}

(() => {
    const searchDiv = document.querySelector(".stork-wrapper");
    const inputBox = searchDiv.querySelector("input");
    /* I am sorry. */
    const root = document.querySelector("#roothack").getAttribute("href");
    let path = document.location.pathname;
    path = path.substring(0, path.lastIndexOf("/") + 1);
    const rootPath = document.location.protocol
        + "//" + document.location.host
        + path
        + root;

    function onQueryUpdate(query, results) {
        if (query === "") {
            hideSearch(searchDiv);
            return;
        }
        blowSearchUp(searchDiv);
    }

    function onResultsHidden(query, results) {
        hideSearch(searchDiv);
    }

    inputBox.addEventListener('input', ev => onQueryUpdate(inputBox.value));

    /* TODO(arsen): SRI for this, needs upstream support */
    stork.initialize("https://files.stork-search.net/releases/v1.4.0/stork.wasm")
    stork.register("wiki", rootPath + "/searchidx.st", {
        "onQueryUpdate": onQueryUpdate,
        "onResultsHidden": onResultsHidden,
    });

    /* show the search again */
    document.querySelector(".stork-wrapper").style.display = "grid";
})();

/* vim: set sw=4 et : */
