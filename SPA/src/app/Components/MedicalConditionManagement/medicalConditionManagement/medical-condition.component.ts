import {Component} from '@angular/core';
import {MedicalConditionService} from "../../../services/MedicalConditionService/medicalConditionService";
import {Router} from "@angular/router";

@Component({
  selector: 'app-medical-condition-management',
  templateUrl: './medical-condition.component.html',
  styleUrls: ['./medical-condition.component.css'],
})

export class MedicalConditionManagementComponent {

  public medicalConditions: any[] = [];

  constructor(private medicalConditionService: MedicalConditionService, private router: Router) {
  }

  ngOnInit() {
    this.loadMedicalConditions();
  }

  private loadMedicalConditions() {
    this.medicalConditionService.getAllMedicalConditions().subscribe(
      (medicalConditions) => {
        console.log('Medical Conditions:', medicalConditions);
        for(let medicalCondition of medicalConditions.medicalConditions) {
          this.medicalConditions.push(medicalCondition);
        }
      },
      (error) => {
        console.error('Failed to load medical conditions:', error);
      }
    );
  }
}
