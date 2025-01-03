import {Component, Inject, OnInit} from '@angular/core';
import {MedicalRecordFreeText} from '../../../Domain/MedicalRecordFreeText';
import {MedicalRecordFreeTextService} from '../../../services/MedicalRecordFreeTextService/medical-record-free-text.service';
import {Router} from '@angular/router';


@Component({
  selector: 'app-add-medical-record-free-text',
  templateUrl: './add-medical-record-free-text.component.html',
  styleUrl: './add-medical-record-free-text.component.css'
})

export class AddMedicalRecordFreeTextComponent implements OnInit {

  freeText: MedicalRecordFreeText = {
    domainId:'',
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
        this.router.navigate(['/staff/patients/medicalRecord']);
        console.log('Success:', response);
      },
      (error) => {
        console.error('Error:', error);
        alert('Error adding medical record comment: ' + (error.error || 'An unknown error occurred.'));
      }
    );
  }

  ngOnInit(): void {
  }


}
