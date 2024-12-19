import { Component } from '@angular/core';
import {MedicalConditionDTO} from '../../../DTOs/general/medicalConditionDTO';
import {MedicalConditionService} from '../../../services/MedicalCondition/medicalConditionService';

@Component({
  selector: 'app-add-medical-condition',
  templateUrl: './add-medical-condition.component.html',
  styleUrl: './add-medical-condition.component.css'
})


export class AddMedicalConditionComponent {
  medicalConditionDTO: MedicalConditionDTO = {
    code: '',
    designation: '',
    description: '',
    symptomsList: [],
  };

  newSymptom: string = '';

  constructor(private medicalConditionService: MedicalConditionService) {}

  addMedicalCondition() {

    console.log(this.medicalConditionDTO);

    this.medicalConditionService.addMedicalCondition(this.medicalConditionDTO).subscribe(
      (response: any) => {
        alert('Medical Condition added successfully!');
      },
      (error: any) => {
        alert(error || 'An unknown error occurred.');
      }
    );

    alert('Medical Condition added successfully!');
  }

  addSymptom() {
    if (this.newSymptom.trim()) {
      this.medicalConditionDTO.symptomsList.push(this.newSymptom.trim());
      this.newSymptom = '';
    }
  }

  removeSymptom(index: number) {
    this.medicalConditionDTO.symptomsList.splice(index, 1); // Remove symptom by index
  }
}

