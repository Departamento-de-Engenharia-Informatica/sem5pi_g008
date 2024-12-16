import { Component, AfterViewInit, OnInit } from '@angular/core';
import { SurgeryRoomService } from "../../../services/SurgeryRoomService/surgery-room.service";
import json from '../../../appsettings.json';

@Component({
  selector: 'app-connect3d',
  templateUrl: './connect3d.component.html',
  styleUrls: ['./connect3d.component.css'],
  standalone: false
})
export class Connect3dComponent implements OnInit, AfterViewInit {

  iframeUrl: string = json.threeDConfig.url;
  arrayData: any[] = [];
  iframeElement!: HTMLIFrameElement;
  additionalData: { title: string, body: string | number }[][] = [];
  constructor(private surgeryRoomService: SurgeryRoomService) {}

  ngOnInit() {
    this.surgeryRoomService.getSurgeryRooms().subscribe({
      next: (data) => {
        this.arrayData = data;
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });

    this.surgeryRoomService.getRoomInfo().subscribe({
      next: (data) => {
        this.additionalData = data;
      },
      error: (err) => {
        console.error('Error fetching room info:', err);
      }
    });
  }

  ngAfterViewInit() {
    this.iframeElement = document.getElementById('iframe') as HTMLIFrameElement;
    this.sendDataToIframe();
  }

  sendDataToIframe() {
    if (this.iframeElement) {
      this.iframeElement.onload = () => {
        const message = {
          arrayData: this.arrayData,
          additionalData: this.additionalData,
          url: this.iframeUrl + "/hospitalFloor"
        };
        this.iframeElement.contentWindow?.postMessage(message, this.iframeUrl);
      };
    }
  }
}
