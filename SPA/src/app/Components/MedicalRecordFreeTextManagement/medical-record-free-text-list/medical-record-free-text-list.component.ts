import {Component, Inject, Input, OnInit} from '@angular/core';
import {
  MedicalRecordFreeTextService
} from '../../../services/MedicalRecordFreeTextService/medical-record-free-text.service';
import {MedicalRecordFreeTextMapper} from '../../../DTOs/mappers/medicalRecordFreeTextMapper';
import {DisplayMedicalRecordFreeTextDTO} from '../../../DTOs/displayDTOs/displayMedicalRecordFreeTextDTO';
import {Router} from '@angular/router';
import {MedicalRecordFreeText} from '../../../Domain/MedicalRecordFreeText';

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
  public filteredMedicalRecordFreeTexts: DisplayMedicalRecordFreeTextDTO[] = [];
  @Input() public medicalRecordId: string = "";



  ngOnInit(): void {
    this.loadMedicalRecordFreeTexts();
  }

  loadMedicalRecordFreeTexts():void{
    this.medicalRecordFreeTextService
      .listMedicalRecordFreeTextByMedicalRecordId(this.medicalRecordId)
      .subscribe(
        (response) => {
          if (response && response.length > 0) {
            this.medicalRecordFreeTexts = response;
            this.filteredMedicalRecordFreeTexts = [...this.medicalRecordFreeTexts];
            this.errorMessage = "";
          } else {
            this.errorMessage = "No comments found.";
            this.filteredMedicalRecordFreeTexts = [];
          }
        },
        (error) => {
          this.errorMessage = "Failed to load comments.";
          console.error("Failed to load comments:", error);
        }
      );
  }


  public addMedicalRecordFreeText(){
      this.router.navigate(['staff/patients/medicalRecord/addFreeText'], { state: { medicalRecordId: this.medicalRecordId } })

  }


}
