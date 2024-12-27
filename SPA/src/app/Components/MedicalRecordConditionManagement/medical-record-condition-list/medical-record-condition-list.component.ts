import {Component, Inject, Input, OnInit, ViewChild} from '@angular/core';
import { MedicalRecordMedicalConditionService } from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import {DisplayMedicalRecordConditionDTO} from '../../../DTOs/displayDTOs/displayMedicalRecordConditionDTO';
import {EnterFilterNameComponent} from '../../Shared/enter-filter-name/enter-filter-name.component';

@Component({
  selector: 'app-medical-record-condition-list',
  templateUrl: './medical-record-condition-list.component.html',
  styleUrl: './medical-record-condition-list.component.css'
})
export class MedicalRecordConditionListComponent implements OnInit {
  constructor(@Inject(MedicalRecordMedicalConditionService) private medicalRecordConditionService: MedicalRecordMedicalConditionService) {
  }

  public filteredMedicalRecordConditions: DisplayMedicalRecordConditionDTO | null = null;
  public currentFilter: string = '';
  public errorMessage: string = "";
  public medicalRecordConditions: DisplayMedicalRecordConditionDTO[] = [];
  public medicalRecordConditionsAux: DisplayMedicalRecordConditionDTO[] = [];
  @Input() public medicalRecordId: string = "";
  @ViewChild(EnterFilterNameComponent) enterFilterName!: EnterFilterNameComponent;

  ngOnInit(): void {
    this.fetchMedicalRecordConditions();
  }

  fetchMedicalRecordConditions(): void {
    this.medicalRecordConditionService.listMedicalRecordConditionsByMedicalRecordId(this.medicalRecordId)
      .subscribe(
        (response : any) => {
          this.medicalRecordConditions = response.medicalRecordConditions;
          this.medicalRecordConditionsAux = this.medicalRecordConditions;
        },
        (error) => {
          this.errorMessage = error;
          console.error('Failed to load medical record conditions:', error);
        }
      );
  }

  handleSelectedFilter(filter: string): void {
    if(filter === 'Code') {
      this.enterFilterName.open("Code");
    }

    if(filter === 'Designation') {
      this.enterFilterName.open("Designation");
    }
  }

  handleEnterFilterValue(filterValue: string) {
    if (this.currentFilter === 'Code') {
      this.applyCodeFilter(filterValue);
    }
    if (this.currentFilter === 'Designation') {
      this.applyDesignationFilter(filterValue);
    }
  }


        console.error('Failed to load medical conditions:', error);
      }
    );

  handleResetFilter() {
    this.medicalRecordConditions = this.medicalRecordConditionsAux;
    this.errorMessage = "";
    this.currentFilter = '';
    this.filteredMedicalRecordConditions = null;

  }

  applyCodeFilter(filterValue: string) {

    this.medicalRecordConditions = [];

    this.medicalRecordConditionService.filterMedicalRecordConditionsByCode(this.medicalRecordId, filterValue)
      .subscribe(
        (response : any) => {
          this.filteredMedicalRecordConditions = response.filteredMedicalRecordConditions;
          if(this.filteredMedicalRecordConditions )
          this.medicalRecordConditions.push(this.filteredMedicalRecordConditions);
        },
        (error) => {
          this.errorMessage = error;
          console.error('Failed to filter medical record conditions:', error);
        }
      );
  }

  applyDesignationFilter(filterValue: string) {
    this.medicalRecordConditions = [];

    this.medicalRecordConditionService.filterMedicalRecordConditionsByDesignation(this.medicalRecordId, filterValue)
      .subscribe(
        (response : any) => {
          this.filteredMedicalRecordConditions = response.filteredMedicalRecordConditions;
          if(this.filteredMedicalRecordConditions )
            this.medicalRecordConditions.push(this.filteredMedicalRecordConditions);
        },
        (error) => {
          this.errorMessage = error;
          console.error('Failed to filter medical record conditions:', error);
        }
      );
  }

}
