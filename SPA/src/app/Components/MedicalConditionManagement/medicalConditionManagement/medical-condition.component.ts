import { Component } from '@angular/core';
import { MedicalConditionService } from "../../../services/MedicalConditionService/medicalConditionService";
import { Router } from "@angular/router";
import { DisplayMedicalConditionDTO } from "../../../DTOs/displayDTOs/displayMedicalConditionDTO";

@Component({
  selector: 'app-medical-condition-management',
  templateUrl: './medical-condition.component.html',
  styleUrls: ['./medical-condition.component.css'],
})
export class MedicalConditionManagementComponent {

  public medicalConditions: DisplayMedicalConditionDTO[] = [];
  public filteredMedicalConditions: DisplayMedicalConditionDTO[] = [];
  public filterCriteria: 'code' | 'designation' = 'code';
  public filterValue: string = '';

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
        this.filteredMedicalConditions = response; // Initialize with all conditions
      },
      (error) => {
        console.error('Failed to load medical conditions:', error);
      }
    );
  }
  public applyFilter() {
    if (!this.filterValue || this.filterValue.trim() === '') {
      // Se o valor do filtro for vazio ou nulo, exibe todos os registros
      this.filteredMedicalConditions = this.medicalConditions;
      return;
    }

    if (this.filterCriteria === 'code') {
      this.medicalConditionService.filterByCode(this.filterValue).subscribe(
        (response: any) => {
          console.log('Filtered by code:', response);
          this.filteredMedicalConditions = response.data || [];
        },
        (error) => {
          console.error('Failed to filter by code:', error);
          this.filteredMedicalConditions = [];
        }
      );
    } else if (this.filterCriteria === 'designation') {
      this.medicalConditionService.filterByDesignation(this.filterValue).subscribe(
        (response: any) => {
          console.log('Filtered by designation:', response);
          this.filteredMedicalConditions = response.data || [];
        },
        (error) => {
          console.error('Failed to filter by designation:', error);
          this.filteredMedicalConditions = [];
        }
      );
    }
  }

  public addMedicalCondition() {
    this.router.navigate(['admin/medicalConditionManagement/add']);
  }

  public editMedicalCondition(medCond: DisplayMedicalConditionDTO) {
    this.router.navigate(['admin/medicalConditionManagement/edit'], { state: { medicalCondition: medCond } });
  }
}
