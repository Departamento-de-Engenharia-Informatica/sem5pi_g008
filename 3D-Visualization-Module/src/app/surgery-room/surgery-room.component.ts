import {Component, EventEmitter, Input, Output, ViewChild} from '@angular/core';
import * as THREE from 'three';
import { CSG } from 'three-csg-ts';
import { GLTFLoader } from 'three-stdlib';
import * as TWEEN from "three/addons/libs/tween.module.js";
import {PerspectiveCamera, Scene, WebGLRenderer} from 'three';

@Component({
  selector: 'app-surgery-room',
  templateUrl: './surgery-room.component.html',
  styleUrls: ['./surgery-room.component.css']
})
export class SurgeryRoomComponent {
  @Output() tableClicked = new EventEmitter<void>();

  public roomGroup!: THREE.Group;
  public wallTexture: string = 'assets/wall.jpg';
  public floorTexture: string = 'assets/roomFloor.jpg';
  public doorFrontTexture: string = 'assets/door_front.png';
  public doorBackTexture: string = 'assets/door_back.png';

  private wallThickness: number = 0.1;
  private roomWidth!: number;
  private roomHeight!: number;
  private roomDepth!: number;

  public isOperating: boolean = false;
  public isOpenDoor: boolean = false;

  private door!: THREE.Group;
  private scene!: THREE.Scene;
  private camera!: THREE.PerspectiveCamera;
  private renderer!: THREE.WebGLRenderer;
  private mouse = new THREE.Vector2();
  private raycaster = new THREE.Raycaster();

  private medicalTableModel: THREE.Object3D | null = null;

  private doorSize = { width: 1.4, height: 2.5, depth: 0.1,  gap: 0.0465 };

  public createRoom(isOperating: boolean = false,width: number,height:number): void {
    this.roomWidth = width;
    this.roomDepth = width;
    this.roomHeight = height;
    this.isOperating = isOperating;
    this.roomGroup = new THREE.Group();
    this.createRoomWalls();
    this.createRoomFloor();
    this.equipRoomWithSurgicalEquipment();

    if (this.isOperating) {
      this.startSurgery();
      this.createClosedDoor();
    } else {
      this.isOpenDoor = true;
      this.createOpenedDoor();
    }
  }

  private createClosedDoor(): void {
    const doorGeometry = new THREE.BoxGeometry(this.doorSize.width, this.doorSize.height, this.doorSize.depth);

    const sideMaterial = new THREE.MeshStandardMaterial({ color: 0xBBBFB9 });

    let doorTexture = new THREE.TextureLoader().load(this.doorFrontTexture);
    doorTexture.colorSpace = THREE.SRGBColorSpace;

    let frontMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, map: doorTexture });

    doorTexture = new THREE.TextureLoader().load(this.doorBackTexture);
    doorTexture.colorSpace = THREE.SRGBColorSpace;

    let backMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, map: doorTexture });

    let doorMesh = new THREE.Mesh(doorGeometry, [sideMaterial, sideMaterial, sideMaterial, sideMaterial, frontMaterial, backMaterial]);
    doorMesh.translateX(this.doorSize.width / 2.0);
    doorMesh.translateY(-this.doorSize.gap);
    doorMesh.updateMatrix();

    const doorPivot = new THREE.Object3D();
    doorPivot.position.set(0.7, -0.25, this.roomDepth / 2 - this.wallThickness / 2);
    doorPivot.scale.set(-1, 1, 1);
    doorPivot.receiveShadow = true;
    doorPivot.castShadow = true;

    doorPivot.add(doorMesh);
    doorPivot.traverseVisible(function (child) {
      if (child instanceof THREE.Object3D) {
        child.castShadow = true;
        child.receiveShadow = true;
      }
    });
    this.roomGroup.add(doorPivot);
    this.door = doorPivot as THREE.Group;
  }

  private createOpenedDoor(): void {
    const doorGeometry = new THREE.BoxGeometry(this.doorSize.width, this.doorSize.height, this.doorSize.depth);

    const sideMaterial = new THREE.MeshStandardMaterial({ color: 0xBBBFB9 });

    let doorTexture = new THREE.TextureLoader().load(this.doorFrontTexture);
    doorTexture.colorSpace = THREE.SRGBColorSpace;

    let frontMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, map: doorTexture });

    doorTexture = new THREE.TextureLoader().load(this.doorBackTexture);
    doorTexture.colorSpace = THREE.SRGBColorSpace;

    let backMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, map: doorTexture });

    let doorMesh = new THREE.Mesh(doorGeometry, [sideMaterial, sideMaterial, sideMaterial, sideMaterial, frontMaterial, backMaterial]);
    doorMesh.translateX(this.doorSize.width / 2.0);
    doorMesh.translateY(-this.doorSize.gap);
    doorMesh.updateMatrix();

    const doorPivot = new THREE.Object3D();
    doorPivot.position.set(0.7, -0.25, this.roomDepth / 2 - this.wallThickness / 2);
    doorPivot.scale.set(-1, 1, 1);
    doorPivot.receiveShadow = true;
    doorPivot.castShadow = true;

    doorPivot.add(doorMesh);
    doorPivot.rotation.y = Math.PI / 2;
    doorPivot.traverseVisible(function (child) {
      if (child instanceof THREE.Object3D) {
        child.castShadow = true;
        child.receiveShadow = true;
      }
    });
    this.roomGroup.add(doorPivot);
    this.door = doorPivot as THREE.Group;
  }

  private createRoomWalls(): void {
    const texture = new THREE.TextureLoader().load(this.wallTexture);
    texture.wrapS = THREE.RepeatWrapping;
    texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(1, 1);

    const wallMaterial = new THREE.MeshStandardMaterial({ map: texture, side : THREE.DoubleSide });
    const whiteMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, side: THREE.DoubleSide });

    const frontBackWallGeometry = new THREE.BoxGeometry(this.roomWidth, this.roomHeight, this.wallThickness);
    const sideWallGeometry = new THREE.BoxGeometry(this.wallThickness, this.roomHeight, this.roomDepth);
    const doorGeometry = new THREE.BoxGeometry(this.doorSize.width, this.doorSize.height, this.doorSize.depth);

    const frontWall = new THREE.Mesh(frontBackWallGeometry);
    frontWall.position.set(0, 0, this.roomDepth / 2 - this.wallThickness / 2);
    frontWall.receiveShadow = true;
    frontWall.castShadow = false;
    frontWall.updateMatrix();

    let doorHoleMesh = new THREE.Mesh(doorGeometry);
    doorHoleMesh.position.set(0, -0.25, this.roomDepth / 2 - this.wallThickness / 2);
    doorHoleMesh.updateMatrix();

    const frontWallCSG = CSG.fromMesh(frontWall);
    const doorCSG = CSG.fromMesh(doorHoleMesh);
    const resultCSG = frontWallCSG.subtract(doorCSG);
    const resultMesh = CSG.toMesh(resultCSG, frontWall.matrix);
    resultMesh.material = [wallMaterial, wallMaterial, whiteMaterial, whiteMaterial, wallMaterial, wallMaterial];
    resultMesh.castShadow = false;
    resultMesh.receiveShadow = true;

    this.roomGroup.add(resultMesh);

    const backWall = new THREE.Mesh(frontBackWallGeometry, [wallMaterial, wallMaterial, whiteMaterial, whiteMaterial, wallMaterial, wallMaterial]);
    backWall.position.set(0, 0, -this.roomDepth / 2 + this.wallThickness / 2);
    backWall.receiveShadow = true;
    backWall.castShadow = false;
    this.roomGroup.add(backWall);

    const leftWall = new THREE.Mesh(sideWallGeometry, [wallMaterial, wallMaterial, whiteMaterial, whiteMaterial, wallMaterial, wallMaterial]);
    leftWall.position.set(-this.roomWidth / 2 + this.wallThickness / 2, 0, 0);
    leftWall.receiveShadow = true;
    leftWall.castShadow = false;
    this.roomGroup.add(leftWall);

    const rightWall = new THREE.Mesh(sideWallGeometry, [wallMaterial, wallMaterial, whiteMaterial, whiteMaterial, wallMaterial, wallMaterial]);
    rightWall.position.set(this.roomWidth / 2 - this.wallThickness / 2, 0, 0);
    rightWall.receiveShadow = true;
    rightWall.castShadow = false;
    this.roomGroup.add(rightWall);

  }


  private createRoomFloor(): void {
    const texture = new THREE.TextureLoader().load(this.floorTexture);
    texture.wrapS = THREE.RepeatWrapping;
    texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(1, 1);

    const floorMaterial = new THREE.MeshStandardMaterial({ map: texture });
    const floorGeometry = new THREE.BoxGeometry(this.roomWidth, this.wallThickness, this.roomDepth);
    const floor = new THREE.Mesh(floorGeometry, floorMaterial);
    floor.position.set(0, -this.roomHeight / 2, 0);
    floor.receiveShadow = true;
    floor.castShadow = false;
    floor.updateMorphTargets();
    this.roomGroup.add(floor);
  }

  private equipRoomWithSurgicalEquipment(): void {
    this.createMedicalTable();
    this.createMedicalLamp();
  }

  private createMedicalTable(): void {
    const loader = new GLTFLoader();
    loader.load('assets/MedicalTable/table.glb', (gltf) => {
      const medicalTableModel = gltf.scene;
      medicalTableModel.scale.set(9, 9, 9);
      medicalTableModel.position.set(-0.0, 0.8, -3.0);
      medicalTableModel.traverseVisible(function (child) {
        child.castShadow = true;
        child.receiveShadow = true;
      });

      this.roomGroup.add(medicalTableModel);
      this.medicalTableModel = medicalTableModel;
    }, undefined, (error) => {
      console.error('Error loading medical table model:', error);
    });
  }

  private onMouseClick(event: MouseEvent): void {
      this.mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
      this.mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

      this.raycaster.setFromCamera(this.mouse, this.camera);

      if(this.medicalTableModel){
        const intersects = this.raycaster.intersectObjects(this.medicalTableModel.children, true);
        if (intersects.length > 0) {
          this.handleClickEvent();
        }
      }
  }
  private addClickListener(): void {
    window.addEventListener('click', this.onMouseClick.bind(this));
  }

  private handleClickEvent(): void {
    this.tableClicked.emit();
  }

  public startSurgery(): void {
    this.createPatient();
    this.createOperatingStaff();
    this.createLampLight();
    this.closeDoor();
  }

  public stopSurgery(): void {
    this.removePatient();
    this.removeOperatingStaff();
    this.removeLampLight();
    this.openDoor();
  }

  private removePatient(): void {
    const patient = this.roomGroup.getObjectByName('patient');
    if (patient) {
      this.roomGroup.remove(patient);
    }
  }

  private removeOperatingStaff(): void {
    const doctor = this.roomGroup.getObjectByName('doctor');
    if (doctor) {
      this.roomGroup.remove(doctor);
    }

    const nurse = this.roomGroup.getObjectByName('nurse');
    if (nurse) {
      this.roomGroup.remove(nurse);
    }
  }

  private removeLampLight(): void {
    const lampLight = this.roomGroup.getObjectByName('lampLight');
    if (lampLight) {
      this.roomGroup.remove(lampLight);
    }
  }

  private createPatient(): void {
    const loader = new GLTFLoader();
    loader.load('assets/Patient/patient.glb', (gltf) => {
      const patientModel = gltf.scene;
      patientModel.scale.set(0.0115, 0.0115, 0.0115);
      patientModel.position.set(0, -0.3, 0.5);
      patientModel.traverseVisible(function (child) {
        child.castShadow = true;
        child.receiveShadow = true;
      });
      patientModel.name = 'patient';
      this.roomGroup.add(patientModel);
    }, undefined, (error) => {
      console.error('Error loading patient model:', error);
    });
  }

  private createOperatingStaff(): void {
    this.createDoctor();
    this.createNurse();
  }

  private createDoctor(): void {
    const loader = new GLTFLoader();
    loader.load('assets/Doctor/doctor.glb', (gltf) => {
      const doctorModel = gltf.scene;
      doctorModel.scale.set(0.013, 0.013, 0.013);
      doctorModel.position.set(1.0, -1.4, 0.5);
      doctorModel.rotation.y = -Math.PI / 2;
      doctorModel.traverseVisible(function (child) {
        child.castShadow = true;
        child.receiveShadow = true;
      });
      doctorModel.name = 'doctor';
      this.roomGroup.add(doctorModel);
    }, undefined, (error) => {
      console.error('Error loading doctor model:', error);
    });
  }

  private createNurse(): void {
    const loader = new GLTFLoader();
    loader.load('assets/Nurse/nurse.glb', (gltf) => {
      const nurseModel = gltf.scene;
      nurseModel.scale.set(0.0095, 0.0095, 0.0095);
      nurseModel.position.set(-1.0, -1.37, 0.5);
      nurseModel.rotation.y = Math.PI / 2;
      nurseModel.traverseVisible(function (child) {
        child.castShadow = true;
        child.receiveShadow = true;
      });
      nurseModel.name = 'nurse';
      this.roomGroup.add(nurseModel);
    }, undefined, (error) => {
      console.error('Error loading nurse model:', error);
    });
  }

  private createMedicalLamp(): void {
    const loader = new GLTFLoader();
    loader.load('assets/SurgicalLight/floorLight.glb', (gltf) => {
      const medicalLampModel = gltf.scene;
      medicalLampModel.scale.set(1.2, 1.2, 1.6);
      medicalLampModel.rotation.x = -Math.PI / 2;
      medicalLampModel.position.set(-1.4, -1.35, -0.2);
      medicalLampModel.traverseVisible(function (child) {
        child.castShadow = true;
        child.receiveShadow = true;
      });
      this.roomGroup.add(medicalLampModel);
    }, undefined, (error) => {
      console.error('Error loading medical lamp model:', error);
    });
  }

  private createLampLight(): void {
    const lampLight = new THREE.SpotLight(0xffffff, 40, 3.4, Math.PI/4); // Increased intensity and smaller cone angle

    lampLight.position.set(0, 1.8, 0.5);
    lampLight.target.position.set(0, 0, 0.5);

    lampLight.castShadow = true;
    lampLight.shadow.mapSize.width = 2048;
    lampLight.shadow.mapSize.height = 2048;

    lampLight.shadow.camera.near = 0.1;
    lampLight.shadow.camera.far = 20;

    lampLight.name = 'lampLight';
    lampLight.target.name = 'lampLightTarget';

    this.roomGroup.add(lampLight);
    this.roomGroup.add(lampLight.target);
  }

  public openDoor(): void {

    if (this.door) {
      if (!this.isOpenDoor) {
        this.isOpenDoor = true;

        const tween = new TWEEN.Tween(this.door.rotation);
        tween.to({ y: Math.PI / 2.0 }, 2000 * (1.0 - this.door.rotation.y / (Math.PI / 2.0)));
        tween.startFromCurrentValues();
      }
    }
  }

  public closeDoor(): void {

    if (this.isOpenDoor) {
      this.isOpenDoor = false;

      const tween = new TWEEN.Tween(this.door.rotation)
      tween.to({ y: 0.0 }, 2000 * this.door.rotation.y / (Math.PI / 2.0));
      tween.startFromCurrentValues();
    }
  }

  private animate() {
    requestAnimationFrame(() => this.animate());
    TWEEN.update();
    this.renderer.render(this.scene, this.camera);
  }

  public initializeAnimation(renderer: WebGLRenderer, scene: Scene, camera: PerspectiveCamera) {
    this.renderer = renderer;
    this.scene = scene;
    this.camera = camera;

    this.animate();
    this.addClickListener();
  }

}
