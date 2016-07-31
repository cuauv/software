/* 
  ASLAM Visualization System
  (c) Christopher Goes 2015.
*/

var scene = new THREE.Scene();
var renderer = new THREE.WebGLRenderer({antialias: true});
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

var camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 20000);
camera.position.set(0, 6, 0);
scene.add(camera);

function animate() {
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
}

animate();
