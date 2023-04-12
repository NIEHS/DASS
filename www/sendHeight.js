function getAppHeight() {
		const appHeight = document.body.scrollHeight;
		window.top.postMessage(appHeight, 'https://ntp.niehs.nih.gov');
}
window.addEventListener('load', getAppHeight);
window.addEventListener('resize', getAppHeight);