import {AfterViewInit, Component, OnInit, ViewChild, ViewContainerRef} from '@angular/core';
import * as THREE from 'three';
import {OrbitControls} from 'three/examples/jsm/controls/OrbitControls.js';
import {GUI} from 'three/addons/libs/lil-gui.module.min.js';
import {SurgeryRoomComponent} from '../surgery-room/surgery-room.component';
import {ValidateMap} from '../validateMap/validateMap';
import json from '../../floorLayout/floorLayout.json';
import {CorridorComponent} from '../corridor/corridor.component';
import {EdgeWallComponent} from '../edge-wall/edge-wall.component';

@Component({
  selector: 'app-operation-floor',
  templateUrl: './operation-floor.component.html',
  styleUrls: ['./operation-floor.component.css']
})
export class OperationFloorComponent implements OnInit, AfterViewInit {
  @ViewChild('floorContainer', {read: ViewContainerRef}) floorContainer!: ViewContainerRef;

  private renderer!: THREE.WebGLRenderer;
  private scene!: THREE.Scene;
  private camera!: THREE.PerspectiveCamera;
  private controls!: OrbitControls;
  private orbitTarget!: THREE.Vector3;
  private targetMarker!: THREE.Mesh;
  private ambientLight!: THREE.AmbientLight;
  private directionalLight!: THREE.DirectionalLight;
  private roomNumber = 0;

  private rooms: { id: number; roomComponent: SurgeryRoomComponent }[] = [];
  private gui!: GUI;
  private map: number[][] = json.map;
  private width: number = json.size.width;
  private height: number = json.size.height;

  constructor(private validateMapService: ValidateMap) {
  }


  ngOnInit(): void {
    try {
      this.validateMapService.validate(this.map);
      this.initialize();
    } catch (error) {
      console.error("An error occurred during initialization:", error);
    }
  }

  private initialize(): void {
    this.initScene();
    this.initRenderer();
    this.initCamera();
    this.initGUI();
  }

  private initScene(): void {
    this.scene = new THREE.Scene();
    this.createAmbientLight();
    this.createDirectionalLight();
  }

  private initGUI(): void {
    this.gui = new GUI();
    this.createCameraGUI();
    this.createOrbitTargetGUI();
    this.ambientLightGUI();
    this.directionLightGUI();
    this.gui.close();
  }

  private initRenderer(): void {
    this.renderer = new THREE.WebGLRenderer();
    this.renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(this.renderer.domElement);
  }

  private initCamera(): void {
    const coords = this.getCoordsOfMiddleOfTheMap();
    this.camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
    this.camera.position.set(coords.x, 15 * this.height, coords.z);
    this.camera.lookAt(new THREE.Vector3(coords.x, 0, coords.z));
    this.orbitTarget = new THREE.Vector3(coords.x, this.height, coords.z);
  }

  ngAfterViewInit(): void {
    this.createRooms();
    this.createCorridorsAndEdgeWalls();
    this.setupCameraControls();
    this.render();
  }

  private createAmbientLight(): void {
    this.ambientLight = new THREE.AmbientLight(0xffffff, 0.2);
    this.scene.add(this.ambientLight);
  }

  private createRooms(): void {
    let newMap = this.map.map(row => [...row]);

    for (let i = 0; i < newMap.length - 1; i++) {
      for (let j = 0; j < newMap[i].length - 1; j++) {
        if (newMap[i][j] === 1) {
          if (newMap[i + 1][j] === 1 &&
            newMap[i][j + 1] === 1 &&
            newMap[i + 1][j + 1] === 1) {

            newMap[i][j] = 0;
            newMap[i + 1][j] = 0;
            newMap[i][j + 1] = 0;
            newMap[i + 1][j + 1] = 0;

            this.createRoom(i, j);
          } else {
            throw new Error(`Invalid room structure at (${i}, ${j})`);
          }
        }
      }
    }
  }

  private createCorridorsAndEdgeWalls(): void {
    for (let i = 0; i < this.map.length; i++) {
      for (let j = 0; j < this.map[i].length; j++) {
        if (this.map[i][j] === 2) {
          this.createCorridor(i, j);
        } else if (this.map[i][j] === 3) {
          console.log("Creating edge wall at", i, j);
          this.createCorridor(i, j);
          this.createEdge(i, j);
        }
      }
    }
  }


  private createEdge(i: number, j: number): void {
    const walls = {
      left: i === 0,
      right: i === this.map.length - 1,
      back: j === 0,
      front: j === this.map[0].length - 1
    };

    const componentRef = this.floorContainer.createComponent(EdgeWallComponent);
    const edgeComponent = componentRef.instance;

    edgeComponent.createEdgeWalls(walls, this.width / 2, this.height);

    const edgeGroup = edgeComponent.edgeWallGroup;
    edgeGroup.position.set((i - 0.5) * this.width / 2, 0, (j - 0.5) * this.width / 2);
    this.scene.add(edgeGroup);
  }

  private createRoom(i: number, j: number): void {
    this.roomNumber++;
    const componentRef = this.floorContainer.createComponent(SurgeryRoomComponent);
    const roomComponent = componentRef.instance;

    roomComponent.createRoom(this.roomNumber % 2 === 0, this.width, this.height);

    const roomGroup = roomComponent.roomGroup;
    roomGroup.position.set(i * this.width / 2, 0, j * this.width / 2);
    roomGroup.rotation.y = this.findClosestRotation(i, j)
    this.scene.add(roomGroup);
    this.rooms.push({id: i, roomComponent});

    const roomFolder = this.gui.addFolder(`Room ${this.roomNumber}`);
    roomFolder.close();
  }

  private createCorridor(i: number, j: number): void {
    const componentRef = this.floorContainer.createComponent(CorridorComponent);
    const corridorComponent = componentRef.instance;

    corridorComponent.createCorridor(this.width / 2);

    const corridorGroup = corridorComponent.corridorGroup;
    corridorGroup.position.set((i - 0.5) * this.width / 2, -this.height / 2, (j - 0.5) * this.width / 2);
    this.scene.add(corridorGroup);

  }

  private findClosestRotation(i: number, j: number): number {

    const isAdjacentToCorridor = (x: number, y: number): boolean => {
      if (x < 0 || y < 0 || x >= this.map.length || y >= this.map[0].length) {
        return false;
      }
      return this.map[x][y] === 2 || this.map[x][y] === 3;
    };

    const cellsToCheck = [
      [i, j],
      [i + 1, j],
      [i, j + 1],
      [i + 1, j + 1]
    ];

    for (const [x, y] of cellsToCheck) {
      if (isAdjacentToCorridor(x - 1, y)) {
        return this.calculateRotation(x, y, x - 1, y);
      }
      if (isAdjacentToCorridor(x + 1, y)) {
        return this.calculateRotation(x, y, x + 1, y);
      }
      if (isAdjacentToCorridor(x, y - 1)) {
        return this.calculateRotation(x, y, x, y - 1);
      }
      if (isAdjacentToCorridor(x, y + 1)) {
        return this.calculateRotation(x, y, x, y + 1);
      }
    }
    return 0;
  }

  private calculateRotation(x: number, y: number, x1: number, y1: number): number {
    if (x === x1 && y < y1) {
      return 0;
    }
    if (x === x1 && y > y1) {
      return Math.PI;
    }
    if (y === y1 && x < x1) {
      return Math.PI / 2;
    }
    if (y === y1 && x > x1) {
      return -Math.PI / 2;
    }
    return 0;
  }

  private setupCameraControls(): void {
    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableZoom = true;
    this.controls.enableRotate = true;
    this.controls.enablePan = false;

    this.controls.target.copy(this.orbitTarget);
    this.controls.update();

    this.controls.mouseButtons.LEFT = THREE.MOUSE.PAN;
    this.controls.mouseButtons.RIGHT = THREE.MOUSE.ROTATE;
  }

  private createCameraGUI(): void {
    const cameraFolder = this.gui.addFolder('Camera');
    cameraFolder.add(this.camera.position, 'x', -50, 50).name('X');
    cameraFolder.add(this.camera.position, 'y', -50, 50).name('Y');
    cameraFolder.add(this.camera.position, 'z', -50, 50).name('Z');
    cameraFolder.close();
  }

  private ambientLightGUI(): void {
    const lightFolder = this.gui.addFolder('Ambient Light');
    lightFolder.add(this.ambientLight, 'intensity', 0, 3).name('Intensity');
    lightFolder.close();
  }

  private directionLightGUI(): void {
    const lightFolder = this.gui.addFolder('Directional Light');
    lightFolder.add(this.directionalLight, 'intensity', 0, 3).name('Intensity');
    lightFolder.close();
  }

  private createOrbitTargetGUI(): void {
    const targetFolder = this.gui.addFolder('Orbit Target');
    targetFolder.add(this.orbitTarget, 'x', -50, 50).name('X').onChange(() => this.updateOrbitTarget());
    targetFolder.add(this.orbitTarget, 'y', -50, 50).name('Y').onChange(() => this.updateOrbitTarget());
    targetFolder.add(this.orbitTarget, 'z', -50, 50).name('Z').onChange(() => this.updateOrbitTarget());
    targetFolder.close();
  }

  private updateOrbitTarget(): void {
    this.controls.target.copy(this.orbitTarget);
    this.targetMarker.position.copy(this.orbitTarget);
    this.controls.update();
  }

  private createDirectionalLight(): void {
    this.directionalLight = new THREE.DirectionalLight(0xffffff, 1);
    this.directionalLight.position.set(0, 10, 0);
    this.directionalLight.target.position.set(0, 0, 0);
    this.directionalLight.castShadow = true;

    this.scene.add(this.directionalLight);
  }

  private getCoordsOfMiddleOfTheMap(): { x: number, z: number } {
    return {
      x: this.width/2 * (this.map.length) / 2 -this.width/2,
      z: this.width/2 * (this.map[0].length) / 2 -this.width/2
    };
  }

  private render = () => {
    this.controls.update();
    this.renderer.render(this.scene, this.camera);
    requestAnimationFrame(this.render);
  };
}
