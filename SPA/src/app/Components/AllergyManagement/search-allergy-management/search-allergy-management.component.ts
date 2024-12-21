import {Component, Inject, OnInit, ViewChild} from '@angular/core';
import {AuthService} from '../../../services/AuthService/auth.service';
import {AllergyService} from '../../../services/AllergyService/allergyService';
import {AllergyMapper} from '../../../DTOs/mappers/allergyMapper';
import {DisplayAllergyDTO} from '../../../DTOs/displayDTOs/displayAllergyDTO';
import {EnterFilterNameComponent} from '../../Shared/enter-filter-name/enter-filter-name.component';

@Component({
  selector: 'app-search-allergy-management',
  templateUrl: './search-allergy-management.component.html',
  styleUrl: './search-allergy-management.component.css'
})

export class SearchAllergyManagementComponent implements OnInit {
  private auth: AuthService;
  private allergyService: AllergyService;
  public allergies: DisplayAllergyDTO[] = [];
  public showFilterButton: boolean = true;
  public showResetFilterButton: boolean = false;
  public showNoAllergiesFound: boolean = false;
  public showAllergiesList: boolean = true;
  public showEmptyFilterError: boolean = false;
  allergy:string='';
  allergyDTO: DisplayAllergyDTO={
    allergy: '',
    effect:''
  };
  currentFilter: string = '';
  @ViewChild(EnterFilterNameComponent) enterFilter!: EnterFilterNameComponent;


  constructor(@Inject(AuthService) auth: AuthService, @Inject(AllergyService) allergyService: AllergyService) {
    this.auth = auth;
    this.allergyService = allergyService;
  }



  private loadAllergies() {
    this.allergyService.getAllAllergies().subscribe(
      (allergies) => {
        console.log('Allergies:', allergies);
        for(let allergy of allergies.allergies) {
          this.allergies.push(AllergyMapper.domainToDisplayDto(allergy));
        }
      },
      (error) => {
        console.error('Failed to load allergies:', error);
      }
    );
  }

  ngOnInit() {
    this.loadAllergies();
  }

  public resetFilter() {
    this.allergies = [];
    this.showFilterButton = true;
    this.showAllergiesList = true;
    this.showEmptyFilterError = false;
    this.loadAllergies();
    this.showNoAllergiesFound = false;
    this.showResetFilterButton = false;
  }

  public applyFilter(filter: string) {
    const trimmedFilter = filter.trim();

    this.allergyService.searchAllergies(trimmedFilter).subscribe(
      (data: { allergies: DisplayAllergyDTO }) => {
        this.showNoAllergiesFound = false;
        this.showAllergiesList = true;
        this.allergies = [];
        this.allergies.push(data.allergies);
        this.showResetFilterButton = true;
      },
      (error) => {
        if (error.status === 404) {
          this.allergies = [];
          this.showAllergiesList = false;
          this.showNoAllergiesFound = true;
          this.showResetFilterButton = true;
        } else {
          this.showAllergiesList = false;
          this.showEmptyFilterError = true;
          this.showResetFilterButton = true;
        }
      }
    );
  }

  public handleFilterSelection(filterValue: string) {
    this.showFilterButton = false;
    this.showEmptyFilterError = false;

    if (this.currentFilter === 'allergy') {
      this.applyFilter(filterValue);
    }
  }

  public handleSelectedFilter(filter: string) {
    if (filter === 'Allergy') {
      this.enterFilter.open("allergy");
    }
  }
}
