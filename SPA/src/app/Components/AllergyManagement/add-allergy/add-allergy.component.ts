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
    allergyCode: '',
    allergyDesignation: '',
    allergyDescription: '',
    allergyEffects: []
  };

  newEffect: string = '';


  constructor(private allergyService: AllergyService, private router: Router) {}

  addAllergy() {

    this.allergyService.addAllergy(this.allergyDTO).subscribe(
      (response: any) => {
        alert('Allergy added successfully!');
        this.router.navigate(['/admin/allergyManagement']);
      },
      (error: any) => {
        alert(error || 'An unknown error occurred.');
      }
    );
  }

  addEffect() {
    if (this.newEffect.trim()) {
      this.allergyDTO.allergyEffects.push(this.newEffect.trim());
      this.newEffect = '';
    }
  }

  removeEffect(index: number) {
    this.allergyDTO.allergyEffects.splice(index, 1);
  }

}
