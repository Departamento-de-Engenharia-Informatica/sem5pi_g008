import { Component, Inject, Input, OnInit, ViewChild } from '@angular/core';
import { MedicalRecordMedicalConditionService } from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import { DisplayMedicalRecordConditionDTO } from '../../../DTOs/displayDTOs/displayMedicalRecordConditionDTO';
import { EnterFilterNameComponent } from '../../Shared/enter-filter-name/enter-filter-name.component';
import {Router} from '@angular/router';

@Component({
  selector: 'app-medical-record-condition-list',
  templateUrl: './medical-record-condition-list.component.html',
  styleUrls: ['./medical-record-condition-list.component.css']
})
export class MedicalRecordConditionListComponent implements OnInit {
  constructor(@Inject(MedicalRecordMedicalConditionService) private medicalRecordConditionService: MedicalRecordMedicalConditionService,private router: Router) {}

  public filteredMedicalRecordConditions: DisplayMedicalRecordConditionDTO | null = null;
  public currentFilter: string = '';
  public errorMessage: string = '';
  public medicalRecordConditions: DisplayMedicalRecordConditionDTO[] = [];
  public medicalRecordConditionsAux: DisplayMedicalRecordConditionDTO[] = [];

  @Input() public medicalRecordId: string = '';
  @ViewChild(EnterFilterNameComponent) enterFilterName!: EnterFilterNameComponent;

  ngOnInit(): void {
    this.fetchMedicalRecordConditions();
  }

  fetchMedicalRecordConditions(): void {
    this.medicalRecordConditionService.listMedicalRecordConditionsByMedicalRecordId(this.medicalRecordId)
      .subscribe(
        (response: any) => {
          this.medicalRecordConditions = response.medicalRecordConditions || [];
          this.medicalRecordConditionsAux = [...this.medicalRecordConditions];
          console.log('Medical record conditions:', this.medicalRecordConditions);

        },
        (error) => {
          this.errorMessage = error;
          console.error('Failed to load medical record conditions:', error);
        }
      );
  }

  redirectToEdit(doaminId: string | undefined) {
    this.router.navigate(['staff/UpdateMedicalRecordConditionComponent'], {
      queryParams: { id: doaminId }
    });
  }

  handleSelectedFilter(filter: string): void {
    this.currentFilter = filter;
    if (filter === 'Code' || filter === 'Designation') {
      this.enterFilterName.open(filter);
    }
  }

  handleEnterFilterValue(filterValue: string): void {
    if (this.currentFilter === 'Code') {
      this.applyCodeFilter(filterValue);
    } else if (this.currentFilter === 'Designation') {
      this.applyDesignationFilter(filterValue);
    }
  }

  handleResetFilter() {
    this.medicalRecordConditions = this.medicalRecordConditionsAux;
    this.errorMessage = "";
    this.currentFilter = '';
    this.filteredMedicalRecordConditions = null;

  }

  applyCodeFilter(filterValue: string): void {
    this.medicalRecordConditionService.filterMedicalRecordConditionsByCode(this.medicalRecordId, filterValue)
      .subscribe(
        (response: any) => {
          this.filteredMedicalRecordConditions = response.filteredMedicalRecordConditions;
          this.medicalRecordConditions = this.filteredMedicalRecordConditions ? [this.filteredMedicalRecordConditions] : [];
        },
        (error) => {
          this.errorMessage = error;
          console.error('Failed to filter medical record conditions:', error);
        }
      );
  }

  applyDesignationFilter(filterValue: string): void {
    this.medicalRecordConditionService.filterMedicalRecordConditionsByDesignation(this.medicalRecordId, filterValue)
      .subscribe(
        (response: any) => {
          this.filteredMedicalRecordConditions = response.filteredMedicalRecordConditions;
          this.medicalRecordConditions = this.filteredMedicalRecordConditions ? [this.filteredMedicalRecordConditions] : [];
        },
        (error) => {
          this.errorMessage = error;
          console.error('Failed to filter medical record conditions:', error);
        }
      );
  }
}
