import { Component } from '@angular/core';
import {MedicalConditionService} from '../../../services/MedicalConditionService/medicalConditionService';
import {Router} from '@angular/router';
import {MedicalConditionMapper} from '../../../DTOs/mappers/medicalConditionMapper';
import {MedicalConditionDTO} from '../../../DTOs/GenericDTOs/medicalConditionDTO';

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
  };

  symptomsList: string[] = [];
  newSymptom: string = '';
  description: string = '';


  constructor(private medicalConditionService: MedicalConditionService, private router: Router) {}

  addMedicalCondition() {

    if(this.symptomsList.length > 0) {
      this.medicalConditionDTO.symptomsList = this.symptomsList;
    }

    if(this.description === undefined || this.description!.trim().length === 0) {
      this.medicalConditionDTO.description = "No Description Provided";
    } else {
      this.medicalConditionDTO.description = this.description;
    }

    const medicalCondition = MedicalConditionMapper.dtoToDomain(this.medicalConditionDTO);

    this.medicalConditionService.addMedicalCondition(medicalCondition).subscribe(
      (response: any) => {
        alert('Medical Condition added successfully!');

        this.router.navigate(['/admin/allergyManagement']);
      },
      (error: any) => {
        alert(error || 'An unknown error occurred.');
      }
    );
  }

  addSymptom() {
    if (this.newSymptom.trim()) {
      this.symptomsList.push(this.newSymptom.trim());
      this.newSymptom = '';
    }
  }

  removeSymptom(index: number) {
    this.symptomsList.splice(index, 1);
  }
}

