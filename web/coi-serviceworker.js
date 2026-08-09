/*! coi-serviceworker v0.1.7 - https://github.com/gzuidhof/coi-serviceworker
    Adds Cross-Origin-Opener-Policy / Cross-Origin-Embedder-Policy headers so the
    page becomes crossOriginIsolated (SharedArrayBuffer, WASM threads) on static
    hosts like GitHub Pages, which cannot send those headers themselves. */
/* eslint-disable */
// credentialless: stay cross-origin isolated (SharedArrayBuffer / WASM threads)
// while still allowing cross-origin CDN resources (js-dos, mermaid) that don't
// send CORP headers. Avoids self-hosting those libraries.
let coepCredentialless = true;
if (typeof window === 'undefined') {
    self.addEventListener("install", () => self.skipWaiting());
    self.addEventListener("activate", (event) => event.waitUntil(self.clients.claim()));

    self.addEventListener("message", (ev) => {
        if (!ev.data) return;
        if (ev.data.type === "deregister") {
            self.registration.unregister().then(() => self.clients.matchAll())
                .then((clients) => clients.forEach((c) => c.navigate(c.url)));
        } else if (ev.data.type === "coepCredentialless") {
            coepCredentialless = ev.data.value;
        }
    });

    // Retry a rejected fetch once — the multi-MB .jsdos bundles occasionally drop
    // mid-download ("TypeError: Failed to fetch"), which otherwise hard-fails the launch.
    const fetchRetry = (req) => fetch(req).catch(() => fetch(req));

    self.addEventListener("fetch", function (event) {
        const r = event.request;
        if (r.cache === "only-if-cached" && r.mode !== "same-origin") return;
        const request = (coepCredentialless && r.mode === "no-cors")
            ? new Request(r, { credentials: "omit" }) : r;
        event.respondWith(
            fetchRetry(request).then((response) => {
                if (response.status === 0) return response;
                const newHeaders = new Headers(response.headers);
                newHeaders.set("Cross-Origin-Embedder-Policy",
                    coepCredentialless ? "credentialless" : "require-corp");
                if (!coepCredentialless) newHeaders.set("Cross-Origin-Resource-Policy", "cross-origin");
                newHeaders.set("Cross-Origin-Opener-Policy", "same-origin");
                return new Response(response.body, { status: response.status, statusText: response.statusText, headers: newHeaders });
            }).catch((e) => console.error(e))
        );
    });
} else {
    (() => {
        const reloadedBySelf = window.sessionStorage.getItem("coiReloadedBySelf");
        window.sessionStorage.removeItem("coiReloadedBySelf");
        const coepDegrading = (reloadedBySelf == "coepdegrade");
        const n = navigator;
        if (n.serviceWorker && n.serviceWorker.controller) {
            n.serviceWorker.controller.postMessage({ type: "coepCredentialless", value: true });
        }
        if (!window.crossOriginIsolated && !coepDegrading && window.isSecureContext !== false) {
            if (!n.serviceWorker) return;
            n.serviceWorker.register(window.document.currentScript.src).then((registration) => {
                registration.addEventListener("updatefound", () => window.location.reload());
                if (registration.active && !n.serviceWorker.controller) window.location.reload();
            }, (err) => console.error("COOP/COEP Service Worker failed to register:", err));
        }
    })();
}
