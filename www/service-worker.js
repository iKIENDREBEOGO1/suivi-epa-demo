importScripts('https://storage.googleapis.com/workbox-cdn/releases/6.5.4/workbox-sw.js');
importScripts('https://unpkg.com/idb@7/build/iife/index-min.js');

// Force immediate activation on first install
workbox.core.skipWaiting();
workbox.core.clientsClaim();

// Cache static (css, js, images, fonts)
workbox.routing.registerRoute(
  ({request}) => ['style','script','image','font'].includes(request.destination),
  new workbox.strategies.CacheFirst({cacheName: 'static-v1'})
);

// Cache snapshot.json + copie dans IndexedDB
const dbPromise = idb.openDB('suivi-epa-demo',1,{upgrade(db){db.createObjectStore('snap')}});

workbox.routing.registerRoute(
  /\/api\/snapshot\.json$/,
  async ({event})=>{
    const resp = await fetch(event.request);      // réseau
    const data = await resp.clone().json();       // clone pour IDB
    (await dbPromise).put('snap', data, 'latest');
    return resp;                                  // stocké aussi par Workbox
  }
);

// Fallback hors-ligne vers offline.html
workbox.routing.setCatchHandler(async ({request})=>{
  if (request.destination === 'document')
    return caches.match('offline.html');
  return Response.error();
});

// (facultatif) message d’installation
self.addEventListener('install', () => console.log('Service Worker installé avec Workbox'));