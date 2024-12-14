import {Component, Inject, OnInit, ViewChild} from '@angular/core';
import {Specialization} from '../../../Domain/Specialization';
import {SpecializationService} from '../../../services/SpecializationService/specialization.service';
import {SpecializationDTO} from '../../../DTO/SpecializationDTO';
import {SpecializationMap} from '../../../Mappers/SpecializationMap';
import {EnterFilterNameComponent} from '../../Shared/enter-filter-name/enter-filter-name.component';

@Component({
  selector: 'app-specialization-management',
  templateUrl: './specialization-management.component.html',
  styleUrl: './specialization-management.component.css'
})
export class SpecializationManagementComponent implements OnInit {
  public errorMessage: string = '';
  public specializationList: Specialization[] = [];
  public specializationListAux: Specialization[] = [];
  private specializationService: SpecializationService;

  @ViewChild(EnterFilterNameComponent) enterFilterName!: EnterFilterNameComponent;

  constructor(@Inject(SpecializationService) specializationService: SpecializationService) {
    this.specializationService = specializationService;
  }

  ngOnInit(): void {

    console.log('Fetching staff profiles');

    this.fetchStaffProfiles();
  }

  public resetFilter() {
    this.errorMessage = '';
    this.specializationList = this.specializationListAux;
  }

  public handleFilter() {
    this.enterFilterName.open('Specialization Name');
  }

  public handleFilterSelection(filterName: string) {

    const trimmedFilterName = filterName.trim();

    if (trimmedFilterName === '') {

      this.specializationList = [];

      this.errorMessage = 'Please enter a name to filter by.';
      return;
    }

    const specializationDTO: SpecializationDTO = {
      specializationName: trimmedFilterName
    };


    this.specializationService.listSpecializationByName(specializationDTO).subscribe(
      (data: SpecializationDTO) => {

        this.specializationList = [];
        this.specializationList.push(this.specializationDTOToSpecialization(data));

        console.log(this.specializationList);

      },
      (error) => {

        if (error.status === 651) {

          this.specializationList = [];

          this.errorMessage = 'Specialization Not Found.';

        } else {
          console.error('Error Searching For a Specialization.');
        }
      }
    );
  }

  public fetchStaffProfiles() {

    this.errorMessage = '';

    this.specializationService.listAllSpecializations().subscribe(
      (data: SpecializationDTO[]) => {

        this.specializationList = this.specializationDTOListToSpecializationList(data);
        this.specializationListAux = this.specializationList;
      },
      (error) => {

        if (error.status === 650) {

          this.errorMessage = "No Specializations To List.";

        } else {
          console.error('Error fetching staff profiles.');
        }
      }
    );
  }


  private specializationDTOListToSpecializationList(specializationDTOList: SpecializationDTO[]): Specialization[] {

    const specializationList: Specialization[] = [];

    for (let specializationDTO of specializationDTOList) {
      specializationList.push(this.specializationDTOToSpecialization(specializationDTO));
    }

    return specializationList;
  }

  private specializationDTOToSpecialization(specializationDTO: SpecializationDTO): Specialization {
    return SpecializationMap.toDomain(specializationDTO);
  }
}
