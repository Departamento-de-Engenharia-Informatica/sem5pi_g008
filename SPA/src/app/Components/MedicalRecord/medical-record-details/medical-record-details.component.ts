import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-medical-record-details',
  templateUrl: './medical-record-details.component.html',
  styleUrl: './medical-record-details.component.css'
})
export class MedicalRecordDetailsComponent implements OnInit{

  medicalRecordId: string = "";

  constructor() {
  }

  ngOnInit(): void {
    const navigationState = history.state;

    if (navigationState && navigationState.medicalRecord) {
      this.medicalRecordId = navigationState.medicalRecord;
      console.log("Received medical record: ", this.medicalRecordId);
    } else {
      console.log("No medical record data available in state");
    }
  }
}
