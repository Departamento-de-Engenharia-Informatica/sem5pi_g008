import { Component } from '@angular/core';
import { MedicalConditionService } from "../../../services/MedicalConditionService/medicalConditionService";
import { Router } from "@angular/router";
import {DisplayMedicalConditionDTO} from "../../../DTOs/displayDTOs/displayMedicalConditionDTO";

@Component({
  selector: 'app-medical-condition-management',
  templateUrl: './medical-condition.component.html',
  styleUrls: ['./medical-condition.component.css'],
})
export class MedicalConditionManagementComponent {

  public medicalConditions: DisplayMedicalConditionDTO[] = [];

  constructor(
      private medicalConditionService: MedicalConditionService,
      private router: Router
  ) {}

  ngOnInit() {
    this.loadMedicalConditions();
  }

  private loadMedicalConditions() {
    this.medicalConditionService.getAllMedicalConditions().subscribe(
        (response) => {
          console.log('Medical conditions loaded:', response);
            this.medicalConditions = response;
        },
        (error) => {
          console.error('Failed to load medical conditions:', error);
        }
    );
  }

  public addMedicalCondition() {
    this.router.navigate(['admin/medicalConditionManagement/add']);
  }

  public editMedicalCondition(medCond: DisplayMedicalConditionDTO) {
    this.router.navigate(['admin/medicalConditionManagement/edit'], { state : {medicalCondition : medCond }});
  }
}
