function getAppHeight() {
		const appHeight = document.body.scrollHeight;
		window.top.postMessage(appHeight, '*');
}
window.addEventListener('load', getAppHeight);
window.addEventListener('resize', getAppHeight);