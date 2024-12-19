import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { AllergyService } from '../../../services/AllergyService/allergyService';
import {AllergyMapper} from '../../../DTOs/mappers/allergyMapper';
import {CreateAllergyDTO} from '../../../DTOs/createDTOs/createAllergyDTO';

@Component({
  selector: 'app-add-allergy',
  templateUrl: './add-allergy.component.html',
  styleUrl: './add-allergy.component.css'
})
export class AddAllergyComponent {

  allergyDTO: CreateAllergyDTO = {
    allergy: '',
  };

  constructor(private allergyService: AllergyService, private router: Router) {}

  addAllergy() {

    const allergy = AllergyMapper.createDtoToDomain(this.allergyDTO);

    this.allergyService.addAllergy(allergy).subscribe(
      (response: any) => {
        alert('Allergy added successfully!');
        this.router.navigate(['/admin/allergyManagement']);
      },
      (error: any) => {
        alert(error || 'An unknown error occurred.');
      }
    );
  }
}
