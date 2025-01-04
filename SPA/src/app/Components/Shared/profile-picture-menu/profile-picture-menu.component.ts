import {Component, inject} from '@angular/core';
import {AuthService} from '../../../services/AuthService/auth.service';
import {saveAs} from 'file-saver';
import {MedicalRecordService} from '../../../services/MedicalRecordService/medicalRecordService';
import {
  MedicalRecordMedicalConditionService
} from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import {
  MedicalRecordAllergyService
} from '../../../services/medicalRecordAllergyService/medical-record-allergy.service';
import {
  MedicalRecordFreeTextService
} from '../../../services/MedicalRecordFreeTextService/medical-record-free-text.service';
import {catchError, forkJoin, map, of, switchMap} from 'rxjs';

@Component({
  selector: 'app-profile-menu',
  templateUrl: './profile-picture-menu.component.html',
  styleUrls: ['./profile-picture-menu.component.css'],
  standalone: false
})
export class ProfilePictureMenuComponent {
  auth = inject(AuthService);
  medicalRecord = inject(MedicalRecordService);
  medicalRecordCondition = inject(MedicalRecordMedicalConditionService);
  medicalRecordAllergy = inject(MedicalRecordAllergyService);
  medicalRecordFreeText = inject(MedicalRecordFreeTextService);
  isMenuOpen = false;
  isPatient = true;

  toggleMenu() {
    this.isMenuOpen = !this.isMenuOpen;
  }


  download() {
    // First, fetch the patient data
    this.medicalRecord.getPatientData().subscribe(
      (patientData) => {

        if (!patientData || !patientData.medicalRecordId) {
          return;
        }

        const medicalRecordId = patientData.medicalRecordId;

        forkJoin({
          patientData: of(patientData),
          conditions: this.medicalRecordCondition.listMedicalRecordConditionsByMedicalRecordId(medicalRecordId).pipe(
            catchError(() => of([]))
          ),
          allergies: this.medicalRecordAllergy.listAllergiesInMedicalRecord(medicalRecordId).pipe(
            catchError(() => of([]))
          ),
          freeText: this.medicalRecordFreeText.listMedicalRecordFreeTextByMedicalRecordId(medicalRecordId).pipe(
            catchError(() => of([]))
          )
        }).subscribe(
          ({patientData, conditions, allergies, freeText}) => {
            const json = JSON.stringify({patientData, conditions, allergies, freeText}, null, 2);
            const filename = 'data.json';

            const blob = new Blob([json], {type: 'application/json'});
            saveAs(blob, filename);
          }
        );
      },
      (error) => {
        // Handle error when fetching patient data
        console.error('Error fetching patient data:', error);
      }
    );

    // Close the menu
    this.isMenuOpen = false;
  }


  logout() {
    this.auth.logout();
    this.isMenuOpen = false;
  }

  constructor() {
    this.auth.getUserRole().subscribe(role => {
      this.isPatient = role?.toLowerCase() === 'patient';
    });
  }
}
