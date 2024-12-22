import {Component, Inject, Input, OnInit} from '@angular/core';
import {
  MedicalRecordMedicalConditionService
} from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import {AllergyMapper} from '../../../DTOs/mappers/allergyMapper';
import {DisplayMedicalRecordConditionDTO} from '../../../DTOs/displayDTOs/displayMedicalRecordConditionDTO';
import {MedicalRecordConditionMapper} from '../../../DTOs/mappers/medicalRecordConditionMapper';

@Component({
  selector: 'app-medical-record-condition-list',
  templateUrl: './medical-record-condition-list.component.html',
  styleUrl: './medical-record-condition-list.component.css'
})
export class MedicalRecordConditionListComponent implements OnInit {
  constructor(@Inject(MedicalRecordMedicalConditionService) private medicalRecordConditionService: MedicalRecordMedicalConditionService) {
  }

  public errorMessage: string = "";
  public medicalRecordConditions: DisplayMedicalRecordConditionDTO[] = [];
  @Input() public medicalRecordId: string = "";

  ngOnInit(): void {
    this.fetchMedicalRecordConditions();
  }

  fetchMedicalRecordConditions() {
    this.medicalRecordConditionService.listMedicalRecordConditionsByMedicalRecordId(this.medicalRecordId).subscribe(
      (medicalRecordConditions) => {
        for (let medicalCondition of medicalRecordConditions.medicalRecordConditions) {

          this.medicalRecordConditions.push(MedicalRecordConditionMapper.domainToDisplayDto(medicalCondition));

        }
      },
      (error) => {

        this.errorMessage = error;

        console.error('Failed to load allergies:', error);
      }
    );
  }


}
