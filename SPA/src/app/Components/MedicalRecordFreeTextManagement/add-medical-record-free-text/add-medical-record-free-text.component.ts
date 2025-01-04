import {Component, Inject, OnInit} from '@angular/core';
import {MedicalRecordFreeText} from '../../../Domain/MedicalRecordFreeText';
import {MedicalRecordFreeTextService} from '../../../services/MedicalRecordFreeTextService/medical-record-free-text.service';
import {Router} from '@angular/router';
import {CreateAllergyDTO} from '../../../DTOs/createDTOs/createAllergyDTO';
import {CreateFreeTextDTO} from '../../../DTOs/createDTOs/createFreeTextDTO';
import {OperationType} from '../../../Domain/OperationType';


@Component({
  selector: 'app-add-medical-record-free-text',
  templateUrl: './add-medical-record-free-text.component.html',
  styleUrl: './add-medical-record-free-text.component.css'
})

export class AddMedicalRecordFreeTextComponent implements OnInit {

  freeText: CreateFreeTextDTO = {
    medicalRecordId: '',
    doctorId: '',
    comment: ''
  };

  constructor(@Inject(MedicalRecordFreeTextService) private medicalRecordFreeTextService: MedicalRecordFreeTextService, private router: Router) {
  }

  isValid():boolean{
    return (this.freeText.doctorId !=='' && this.freeText.comment!=='');
  }

  addMedicalRecordFreeText(){
    this.medicalRecordFreeTextService.addMedicalRecordFreeText(this.freeText).subscribe(
      (response) => {
        alert('Comment added successfully!');
        this.router.navigate(['/staff/patients']);
        console.log('Success:', response);
      },
      (error) => {
        console.error('Error:', error);
        alert('Error adding medical record comment: ' + (error.error || 'An unknown error occurred.'));
      }
    );
  }


  ngOnInit() {
    const medicalRecordId = history.state.medicalRecordId;
    if (medicalRecordId) {
      console.log(medicalRecordId);
      this.freeText.medicalRecordId = medicalRecordId;
    } else {
      console.error('Medical record data not found in router state.');
      alert('Error loading comments details.');
    }
  }


}
