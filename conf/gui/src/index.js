import $ from 'jquery';
import * as THREE from 'three';
import _ from 'lodash';
import OrbitControls from 'three-orbitcontrols';

let camera, scene, renderer;
let controls;
let vehicleConfig = {};
let hiddenComponents = {};
const PLANE_COLORS = [0xff0000, 0x00ff00, 0x0000ff, 0xffff00];
let planeColorIndex = 0;

$(document).ready(() => {
    init();
    animate();
})


function init() {
    const container = document.getElementById('threejs-container');
    camera = new THREE.PerspectiveCamera(70, window.innerWidth / window.innerHeight, 0.01, 100);
    camera.up = new THREE.Vector3(0, 0, -1);

    controls = new OrbitControls(camera, container);
    controls.target = new THREE.Vector3(0, 0, 0);
    controls.enableKeys = false;
    scene = new THREE.Scene();

    let geometry = new THREE.SphereGeometry(0.04, 16, 16);
    let material = new THREE.MeshPhongMaterial({ color: 'gray' });
    material.transparent = true;
    material.opacity = 0.9;

    let mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(0, 0, 0);
    mesh.rotation.z = Math.PI / 2;
    mesh.castShadow = true;
    addMesh('submarine', mesh);

    const axesHelper = new THREE.AxesHelper(5);
    axesHelper.position.set(1.0, 0, 0.01);
    scene.add(axesHelper);

    const ambientLight = new THREE.AmbientLight(0x0c0c0c);
    ambientLight.intensity = 7;
    scene.add(ambientLight);

    let spotLight = new THREE.SpotLight(0xffffff);
    spotLight.position.set(0, -5, -3);
    spotLight.castShadow = true;
    spotLight.intensity = 0.9;
    scene.add(spotLight);
    spotLight = new THREE.SpotLight(0xffffff);
    spotLight.position.set(0, 5, -3);
    spotLight.castShadow = true;
    spotLight.intensity = 0.9;
    scene.add(spotLight);

    material = new THREE.MeshPhongMaterial( { color: 0x808080, side: THREE.DoubleSide } );

    geometry = new THREE.PlaneBufferGeometry( 2000, 2000 );

    mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(0, 0, 0.4);
    mesh.receiveShadow = true;
    addMesh('ground-plane', mesh);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(container.clientWidth, container.clientHeight);
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.BasicShadowMap;
    container.appendChild(renderer.domElement);

    // controls.update() must be called after any manual changes to the camera's transform
    camera.position.set(-1, 1, 0);
    controls.update();

    initConfig();

    $('#export-btn').click(() => {
        const exportConfig = _.pick(vehicleConfig, [
            'buoyancy_force',
            'gravity_force',
            'drag_planes',
            'uncompensated_drag_planes',
            'thrusters',
            'cameras',
        ]);
        const configText = JSON.stringify(exportConfig, null, 4);
        console.log(configText);
        // Create hidden element to fill with the text we want to copy
        const $tmp = $('<textarea>').attr({ display: 'none' });
        $('body').append($tmp);
        $tmp.val(configText).select();
        // Trigger copy command on hidden element
        document.execCommand('copy');
        $tmp.remove();
        alert(`Copied to the clipboard: \n\n${configText}`);
    });
}

function animate() {

    requestAnimationFrame(animate);

    // required if controls.enableDamping or controls.autoRotate are set to true
    //controls.update();
    renderer.render(scene, camera);

}

function addMesh(name, mesh) {
    removeMesh(name);
    if (!isVisible(name)) {
        return;
    }
    mesh.name = name;
    scene.add(mesh);
}

function removeMesh(name) {
    const mesh = scene.getObjectByName(name);
    if (mesh) {
        scene.remove(mesh);
    }
}

function addDragPlane(idx, width, height, drag_coeff, pos, normal) {
    const orientationQuat = new THREE.Quaternion().setFromUnitVectors(new THREE.Vector3(0, 0, 1), normal);
    const geometry = new THREE.PlaneGeometry(width, height);
    const color = nextPlaneColor();
    const material = new THREE.MeshBasicMaterial({color: color, side: THREE.DoubleSide});
    material.transparent = true;
    material.opacity = 0.9;
    const plane = new THREE.Mesh(geometry, material);
    plane.position.set(pos.x, pos.y, pos.z);
    plane.applyQuaternion(orientationQuat);
    addMesh(`drag-plane-${idx}`, plane);
    const arrowHelper = new THREE.ArrowHelper(normal, pos, drag_coeff * width * height, color);
    addMesh(`drag-plane-normal-${idx}`, arrowHelper);
}

function nextPlaneColor() {
    const color = PLANE_COLORS[planeColorIndex];
    planeColorIndex = (planeColorIndex + 1) % PLANE_COLORS.length;
    return color;
}

function addThruster(name, pos, heading, pitch) {
    const geometry = new THREE.ConeGeometry(0.05, 0.1, 32);
    const material = new THREE.MeshPhongMaterial({ color: 'green' });
    material.transparent = true;
    material.opacity = 0.9;

    const mesh = new THREE.Mesh(geometry, material);
    mesh.name = name;
    mesh.position.set(pos.x, pos.y, pos.z);
    mesh.rotation.y = pitch * Math.PI / 180;
    mesh.rotation.z = (heading + 90) * Math.PI / 180;
    addMesh(`thruster-${name}`, mesh);
}

function addCamera(name, pos, heading, pitch, roll) {
    const geometry = new THREE.ConeGeometry(0.05, 0.1, 4);
    const material = new THREE.MeshPhongMaterial({ color: 'red' });
    material.transparent = true;
    material.opacity = 0.9;

    const mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(pos.x, pos.y, pos.z);
    mesh.rotation.x = roll * Math.PI / 180;
    mesh.rotation.y = pitch * Math.PI / 180;
    mesh.rotation.z = (heading + 90) * Math.PI / 180;
    addMesh(`camera-${name}`, mesh);
}

function createToggleButton(defaultVisibility, handler) {
    let visible = defaultVisibility;
    const $button = $('<button>').addClass('toggle-btn');
    const render = (visible) => $button.text(visible ? 'Visible' : 'Hidden').css({ color: visible ? 'limegreen' : 'red' });
    $button.click(function() {
        visible = !visible;
        render(visible);
        handler(visible);
    });
    render(visible);
    return $button;
}

function setVisibility(component, visible) {
    if (visible) {
        delete hiddenComponents[component];
    }
    else {
        hiddenComponents[component] = true;
    }
}

function isVisible(component) {
    return !(component in hiddenComponents);
}

function renderBuoyancyForce() {
    const centerOfBuoyancy = new THREE.Vector3().fromArray(vehicleConfig['center_of_buoyancy']);
    const arrowHelper = new THREE.ArrowHelper(new THREE.Vector3(0, 0, -1), centerOfBuoyancy, vehicleConfig['buoyancy_force'] / 1000, 0x3366ff);
    addMesh('buoyancy-force', arrowHelper);
    const $buoyancyForceControls = $('#buoyancy-force-controls');
    $buoyancyForceControls.empty();
    const $sectionHeader = $('<div>').addClass('section-header');
    $buoyancyForceControls.append($sectionHeader);
    $sectionHeader.append($('<span>').text('Buoyancy'));
    $sectionHeader.append(createToggleButton(isVisible('buoyancy-force'), visible => {
        setVisibility('buoyancy-force', visible);
        renderBuoyancyForce();
    }));
    const $bodyContainer = $('<div>').addClass('section-body');
    $buoyancyForceControls.append($bodyContainer);
    const $inputContainer = $('<div>').addClass('input-container');
    $bodyContainer.append($inputContainer);
    const $label = $('<label>').attr({ for: 'buoyancy-force' }).text('Force');
    $inputContainer.append($label);
    const $input = $('<input>').attr({ id: 'buoyancy-force', type: 'text' }).val(vehicleConfig['buoyancy_force']);
    $input.change(function() {
        vehicleConfig['buoyancy_force'] = parseFloat($(this).val());
        renderBuoyancyForce();
    });
    $inputContainer.append($input);
}

function renderGravityForce() {
    // NOTE: we are assuming that the center of gravity is the origin
    const arrowHelper = new THREE.ArrowHelper(new THREE.Vector3(0, 0, 1), new THREE.Vector3(0, 0, 0), vehicleConfig['gravity_force'] / 1000, 0x9900ff);
    addMesh('gravity-force', arrowHelper);
    const $gravityForceControls = $('#gravity-force-controls');
    $gravityForceControls.empty();
    const $sectionHeader = $('<div>').addClass('section-header');
    $gravityForceControls.append($sectionHeader);
    $sectionHeader.append($('<span>').text('Gravity'));
    $sectionHeader.append(createToggleButton(isVisible('gravity-force'), visible => {
        setVisibility('gravity-force', visible);
        renderGravityForce();
    }));
    const $bodyContainer = $('<div>').addClass('section-body');
    $gravityForceControls.append($bodyContainer);
    const $inputContainer = $('<div>').addClass('input-container');
    $bodyContainer.append($inputContainer);
    const $label = $('<label>').attr({ for: 'gravity-force' }).text('Force');
    $inputContainer.append($label);
    const $input = $('<input>').attr({ id: 'gravity-force', type: 'text' }).val(vehicleConfig['gravity_force']);
    $input.change(function() {
        vehicleConfig['gravity_force'] = parseFloat($(this).val());
        renderGravityForce();
    });
    $inputContainer.append($input);
}

function renderDragPlanes() {
    planeColorIndex = 0;
    const dragPlanes = (vehicleConfig['drag_planes'] || []).concat(vehicleConfig['uncompensated_drag_planes'] || []);
    const $dragPlaneControls = $('#drag-plane-controls');
    $dragPlaneControls.empty();
    dragPlanes.filter(plane => plane['area'] !== 0).forEach((plane, idx) => {
        const planePos = new THREE.Vector3().fromArray(plane['pos']);
        const planeNormal = new THREE.Vector3().fromArray(plane['normal']);
        addDragPlane(idx, Math.sqrt(plane['area']), Math.sqrt(plane['area']), plane['cD'], planePos, planeNormal);
        const $container = $('<div>').addClass('drag-plane-control');
        const $sectionHeader = $('<div>').addClass('section-header');
        $sectionHeader.append($('<span>').text(`Drag Plane ${idx + 1}`));
        $sectionHeader.append(createToggleButton(isVisible(`drag-plane-${idx}`), visible => {
            setVisibility(`drag-plane-${idx}`, visible);
            setVisibility(`drag-plane-normal-${idx}`, visible);
            renderDragPlanes();
        }));
        $container.append($sectionHeader);
        const $bodyContainer = $('<div>').addClass('section-body');
        $container.append($bodyContainer);
        const addConfigField = (name, label, getter, setter) => {
            const $inputContainer = $('<div>').addClass('input-container');
            const $label = $('<label>').attr({ for: `drag-${idx}-${name}` }).text(label);
            $inputContainer.append($label);
            const $input = $('<input>').attr({ id: `drag-${idx}-${name}`, type: 'text' }).val(getter());
            $input.change(function() {
                setter(parseFloat($(this).val()));
                renderDragPlanes();
            });
            $inputContainer.append($input);
            $bodyContainer.append($inputContainer);
        }
        addConfigField('area', 'Area', () => plane['area'], val => plane['area'] = val);
        addConfigField('cD', 'Drag coefficient (cD)', () => plane['cD'], val => plane['cD'] = val);
        addConfigField('pos-x', 'X position', () => plane['pos'][0], val => plane['pos'][0] = val);
        addConfigField('pos-y', 'Y position', () => plane['pos'][1], val => plane['pos'][1] = val);
        addConfigField('pos-z', 'Z position', () => plane['pos'][2], val => plane['pos'][2] = val);
        addConfigField('normal-x', 'X normal', () => plane['normal'][0], val => plane['normal'][0] = val);
        addConfigField('normal-y', 'Y normal', () => plane['normal'][1], val => plane['normal'][1] = val);
        addConfigField('normal-z', 'Z normal', () => plane['normal'][2], val => plane['normal'][2] = val);
        $dragPlaneControls.append($container);
    });
}

function renderThrusters() {
    const thrusters = vehicleConfig['thrusters'] || [];
    const $thrusterControls = $('#thruster-controls');
    $thrusterControls.empty();
    thrusters.filter(thruster => !thruster['broken']).forEach((thruster, idx) => {
        const thrusterPos = new THREE.Vector3().fromArray(thruster['pos']);
        const headingPitch = thruster['heading_pitch'];
        addThruster(thruster['name'], thrusterPos, headingPitch[0], headingPitch[1]);
        const $container = $('<div>').addClass('thruster-control');
        const $sectionHeader = $('<div>').addClass('section-header');
        $sectionHeader.append($('<span>').text(`Thruster ${thruster['name']}`));
        $sectionHeader.append(createToggleButton(isVisible(`thruster-${thruster['name']}`), visible => {
            setVisibility(`thruster-${thruster['name']}`, visible);
            renderThrusters();
        }));
        $container.append($sectionHeader);
        const $bodyContainer = $('<div>').addClass('section-body');
        $container.append($bodyContainer);
        const addConfigField = (name, label, getter, setter) => {
            const $inputContainer = $('<div>').addClass('input-container');
            const $label = $('<label>').attr({ for: `thruster-${idx}-${name}` }).text(label);
            $inputContainer.append($label);
            const $input = $('<input>').attr({ id: `thruster-${idx}-${name}`, type: 'text' }).val(getter());
            $input.change(function() {
                setter(parseFloat($(this).val()));
                renderThrusters();
            });
            $inputContainer.append($input);
            $bodyContainer.append($inputContainer);
        }
        addConfigField('pos-x', 'X position', () => thruster['pos'][0], val => thruster['pos'][0] = val);
        addConfigField('pos-y', 'Y position', () => thruster['pos'][1], val => thruster['pos'][1] = val);
        addConfigField('pos-z', 'Z position', () => thruster['pos'][2], val => thruster['pos'][2] = val);
        addConfigField('heading', 'Heading', () => thruster['heading_pitch'][0], val => thruster['heading_pitch'][0] = val);
        addConfigField('pitch', 'Pitch', () => thruster['heading_pitch'][1], val => thruster['heading_pitch'][1] = val);
        $thrusterControls.append($container);
    });
}

function renderCameras() {
    const cameras = vehicleConfig['cameras'] || [];
    const $cameraControls = $('#camera-controls');
    $cameraControls.empty();
    Object.keys(cameras).forEach((cameraName, idx) => {
        const camera = cameras[cameraName];
        const cameraPos = new THREE.Vector3().fromArray(camera['position']);
        const cameraHpr = camera['orientation_hpr'];
        addCamera(cameraName, cameraPos, cameraHpr[0], cameraHpr[1], cameraHpr[2]);
        const $container = $('<div>').addClass('camera-control');
        const $sectionHeader = $('<div>').addClass('section-header');
        $sectionHeader.append($('<span>').text(`Camera ${cameraName}`));
        $sectionHeader.append(createToggleButton(isVisible(`camera-${cameraName}`), visible => {
            setVisibility(`camera-${cameraName}`, visible);
            renderCameras();
        }));
        $container.append($sectionHeader);
        const $bodyContainer = $('<div>').addClass('section-body');
        $container.append($bodyContainer);
        const addConfigField = (name, label, getter, setter) => {
            const $inputContainer = $('<div>').addClass('input-container');
            const $label = $('<label>').attr({ for: `camera-${idx}-${name}` }).text(label);
            $inputContainer.append($label);
            const $input = $('<input>').attr({ id: `camera-${idx}-${name}`, type: 'text' }).val(getter());
            $input.change(function() {
                setter(parseFloat($(this).val()));
                renderCameras();
            });
            $inputContainer.append($input);
            $bodyContainer.append($inputContainer);
        }
        addConfigField('pos-x', 'X position', () => camera['position'][0], val => camera['position'][0] = val);
        addConfigField('pos-y', 'Y position', () => camera['position'][1], val => camera['position'][1] = val);
        addConfigField('pos-z', 'Z position', () => camera['position'][2], val => camera['position'][2] = val);
        addConfigField('heading', 'Heading', () => camera['orientation_hpr'][0], val => camera['orientation_hpr'][0] = val);
        addConfigField('pitch', 'Pitch', () => camera['orientation_hpr'][1], val => camera['orientation_hpr'][1] = val);
        addConfigField('roll', 'Roll', () => camera['orientation_hpr'][2], val => camera['orientation_hpr'][2] = val);
        $cameraControls.append($container);
    });
}

function render() {
    renderGravityForce();
    renderBuoyancyForce();
    renderDragPlanes();
    renderThrusters();
    renderCameras();
}

function initConfig() {
    $.get('/config', (config) => {
        vehicleConfig = JSON.parse(config);
        console.log(vehicleConfig);
        render();
    });
}
