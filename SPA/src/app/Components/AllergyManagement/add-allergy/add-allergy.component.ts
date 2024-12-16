import { Component } from '@angular/core';
import { AllergyDTO } from '../../../DTOs/allergyDTO';
import { Router } from '@angular/router';
import { AllergyService } from '../../../services/AllergyService/allergyService';

@Component({
  selector: 'app-add-allergy',
  templateUrl: './add-allergy.component.html',
  styleUrl: './add-allergy.component.css'
})
export class AddAllergyComponent {

  allergyDTO: AllergyDTO = {
    allergy: '',
  };

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
}
