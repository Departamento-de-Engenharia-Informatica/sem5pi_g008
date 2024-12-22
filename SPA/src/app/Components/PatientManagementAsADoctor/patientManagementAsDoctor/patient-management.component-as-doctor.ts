import {Component, Inject, OnInit} from '@angular/core';
import {Router} from '@angular/router';
import {PatientProfileService} from '../../../services/PatientProfileService/patient-profile-service';
import {DisplayPatientProfileDTO} from '../../../DTOs/displayDTOs/displayPatientProfileDTO';

@Component({
  selector: 'app-patient-management-as-a-doctor',
  templateUrl: './patient-management.component-as-doctor.html',
  styleUrls: ['./patient-management.component-as-doctor.css'],
})
export class PatientManagementAsDoctorComponent implements OnInit {

  protected patientProfileList: DisplayPatientProfileDTO[] = [];

  constructor(
              private patientProfileService: PatientProfileService , private router: Router, ) {
  }

  ngOnInit(): void {
    this.getAllPatients();
  }

  getAllPatients() {
    this.patientProfileService.listAllPatientProfilesDTOs().subscribe(
      (data) => {
        this.patientProfileList = data;
        console.log(this.patientProfileList);
      },
      (error) => {
        console.log(error);
      }
    );

  }

  viewPatientMedicalRecord(id: string) {
    const medicalRecord = {};

    console.log("Viewing patient medical record with id: " + id);

    this.router.navigate(['staff/patients/medicalRecord'], {state: {medicalRecord: id}});
  }

}


