export let webEnvironment = !!window;
export let globalDriver = null;

export function setDriver(driver) {
    return function() {
	globalDriver = driver;
    }
}

export function getDriver() {
    return globalDriver;
}
