import {Component, Inject, OnInit} from '@angular/core';
import {Specialization} from '../../../Domain/Specialization';
import {SpecializationService} from '../../../services/SpecializationService/specialization.service';
import {SpecializationDTO} from '../../../DTO/SpecializationDTO';
import {SpecializationMap} from '../../../Mappers/SpecializationMap';

@Component({
  selector: 'app-specialization-management',
  templateUrl: './specialization-management.component.html',
  styleUrl: './specialization-management.component.css'
})
export class SpecializationManagementComponent implements OnInit {
  public errorMessage: string = '';
  public specializationList: Specialization[] = [];
  private specializationService: SpecializationService;

  constructor(@Inject(SpecializationService) specializationService: SpecializationService) {
    this.specializationService = specializationService;
  }

  ngOnInit(): void {

    console.log('Fetching staff profiles');

    this.fetchStaffProfiles();
  }


  public fetchStaffProfiles() {

    this.errorMessage = '';

    this.specializationService.listAllSpecializations().subscribe(
      (data: SpecializationDTO[]) => {

        this.specializationList = this.specializationDTOListToSpecializationList(data);

      },
      (error) => {

        if(error.status === 650) {

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
