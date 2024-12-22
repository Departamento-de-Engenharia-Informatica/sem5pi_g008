import {Component, OnInit} from '@angular/core';
import {MedicalConditionService} from '../../../services/MedicalConditionService/medicalConditionService';
import {Router} from '@angular/router';
import {MedicalConditionMapper} from '../../../DTOs/mappers/medicalConditionMapper';
import {MedicalConditionDTO} from '../../../DTOs/GenericDTOs/medicalConditionDTO';

@Component({
  selector: 'app-edit-medical-condition',
  templateUrl: './edit-medical-condition.component.html',
  styleUrl: './edit-medical-condition.component.css'
})


export class EditMedicalConditionComponent implements OnInit{

  updatedMedicalCondition!: MedicalConditionDTO;
  originalMedicalCondition!: MedicalConditionDTO;

  newSymptom: string = '';

  constructor(private medicalConditionService: MedicalConditionService, private router: Router) {}

  ngOnInit() {
    const medicalConditionFromState = history.state.medicalCondition;
    if (medicalConditionFromState) {
      this.updatedMedicalCondition = {...medicalConditionFromState};
      this.originalMedicalCondition = {...medicalConditionFromState};
      this.updatedMedicalCondition.symptomsList = [...medicalConditionFromState.symptomsList];
      this.originalMedicalCondition.symptomsList = [...medicalConditionFromState.symptomsList];

      if(!this.updatedMedicalCondition.symptomsList) {
        this.updatedMedicalCondition.symptomsList = [];
        this.originalMedicalCondition.symptomsList = [];
      }

    } else {
      console.error('Medical Condition data not found in router state.');
      alert('Error loading medical condition details.');
      this.router.navigate(['/admin/medicalConditionManagement']);
    }
  }

  editMedicalCondition() {
    if (this.updatedMedicalCondition.description !== this.originalMedicalCondition.description) {
      console.log(`Updating description with ID: ${this.originalMedicalCondition.domainId}`);
    }

    const addedSymptoms = this.updatedMedicalCondition.symptomsList!.filter(symptom => !this.originalMedicalCondition.symptomsList!.includes(symptom));
    const removedSymptoms = this.originalMedicalCondition.symptomsList!.filter(symptom => !this.updatedMedicalCondition.symptomsList!.includes(symptom));

    if (addedSymptoms.length > 0) {
      console.log('Added symptoms:', addedSymptoms);
    }

    if (removedSymptoms.length > 0) {
      console.log('Removed symptoms:', removedSymptoms);
    }

  }

  addSymptom() {
    if (this.newSymptom.trim()) {
      this.updatedMedicalCondition.symptomsList!.push(this.newSymptom.trim());
      this.newSymptom = '';
    }
  }

  removeSymptom(index: number) {
    this.updatedMedicalCondition.symptomsList!.splice(index, 1);
  }
}

