import {Component, Inject, Input, OnInit} from '@angular/core';
import {
  MedicalRecordFreeTextService
} from '../../../services/MedicalRecordFreeTextService/medical-record-free-text.service';
import {MedicalRecordFreeTextMapper} from '../../../DTOs/mappers/medicalRecordFreeTextMapper';
import {DisplayMedicalRecordFreeTextDTO} from '../../../DTOs/displayDTOs/displayMedicalRecordFreeTextDTO';
import {Router} from '@angular/router';

@Component({
  selector:'app-medical-record-free-text-list',
  templateUrl:'./medical-record-free-text-list.component.html',
  styleUrl:'./medical-record-free-text-list.component.css'
})

export class MedicalRecordFreeTextListComponent implements OnInit{

  constructor(@Inject(MedicalRecordFreeTextService) private medicalRecordFreeTextService : MedicalRecordFreeTextService, private router: Router) {
  }


  public errorMessage: string = "";
  public medicalRecordFreeTexts: DisplayMedicalRecordFreeTextDTO[] = [];
  @Input() public medicalRecordId: string = "";



  ngOnInit(): void {
    this.fetchMedicalRecordFreeTexts();
  }

  fetchMedicalRecordFreeTexts() {
    this.medicalRecordFreeTextService.listMedicalRecordFreeTextByMedicalRecordId(this.medicalRecordId).subscribe(
      (medicalRecordFreeTexts) => {
        for (let freeText of medicalRecordFreeTexts.medicalRecordFreeText) {

          this.medicalRecordFreeTexts.push(MedicalRecordFreeTextMapper.domainToDisplayDto(freeText));

        }
      },
      (error) => {

        this.errorMessage = error;
        console.error('Failed to load comments:', error);
      }
    );
  }


  public addMedicalRecordFreeText(){
      this.router.navigate(['staff/patients/medicalRecord/addFreeText'])

  }


}
