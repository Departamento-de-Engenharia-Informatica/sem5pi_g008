import {Component, Inject, OnInit} from '@angular/core';
import {Router} from '@angular/router';
import {PatientProfileService} from '../../../services/PatientProfileService/patient-profile-service';
import {MedicalRecordService} from '../../../services/MedicalRecordService/medical-record.service';
import {DisplayPatientProfileDTO} from '../../../DTOs/displayDTOs/displayPatientProfileDTO';

@Component({
  selector: 'app-patient-management-as-a-doctor',
  templateUrl: './patient-management.component-as-doctor.html',
  styleUrls: ['./patient-management.component-as-doctor.css'],
})
export class PatientManagementAsDoctorComponent implements OnInit {

  private medicalRecordService: MedicalRecordService;
  protected patientProfileList: DisplayPatientProfileDTO[] = [];

  constructor(@Inject(MedicalRecordService) medicalRecordService : MedicalRecordService,
              private patientProfileService: PatientProfileService , private router: Router, ) {
    this.medicalRecordService = medicalRecordService;
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
    console.error("Not implemented yet");
    //SENDS THE MEDICAL RECORD ID TO BE EASIER TO RETRIEVE THE MEDICAL RECORD
    //AND ALL THE INFORMATION RELATED TO IT
    this.router.navigate(['staff/patient/medicalRecord'], {state: {medicalRecord: medicalRecord}});
  }

}


