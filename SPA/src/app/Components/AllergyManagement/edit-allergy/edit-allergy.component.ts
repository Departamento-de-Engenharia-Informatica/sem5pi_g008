import {Component, OnInit} from '@angular/core';
import {DisplayAllergyDTO} from '../../../DTOs/displayDTOs/displayAllergyDTO';
import {Router} from '@angular/router';
import {AllergyService} from '../../../services/AllergyService/allergyService';

@Component({
  selector: 'app-edit-allergy',
  templateUrl: './edit-allergy.component.html',
  styleUrl: './edit-allergy.component.css'
})
export class EditAllergyComponent implements OnInit {

  updatedAllergy!: DisplayAllergyDTO;
  originalAllergy!: DisplayAllergyDTO;

  newEffect: string = '';

  designationDifferent: boolean = false;
  descriptionDifferent: boolean = false;
  effectsDifferent: boolean = false;

  hasError: boolean = false;

  constructor(private allergyService: AllergyService, private router: Router) {}

  ngOnInit() {
    const allergyFromState = history.state.allergy;

    if (allergyFromState) {
      this.updatedAllergy = {...allergyFromState};
      this.originalAllergy = {...allergyFromState};

      this.updatedAllergy.effects = [...allergyFromState.effects];
      this.originalAllergy.effects = [...allergyFromState.effects];


      if(!this.updatedAllergy.effects) {
        this.updatedAllergy.effects = [];
        this.originalAllergy.effects = [];
      }

    } else {
      console.error('Allergy data not found in router state.');
      alert('Error loading Allergy details.');
      this.router.navigate(['/admin/allergyManagement']);
    }
  }


  async updateAllergyDesignation(designation: string): Promise<void> {
    return this.allergyService.updateAllergyDesignation(this.updatedAllergy.domainId!, designation).toPromise();
  }

  async updateAllergyDescription(description: string): Promise<void> {
    return this.allergyService.updateAllergyDescription(this.updatedAllergy.domainId!, description).toPromise();
  }

  async updateAllergyEffects(effects: string[]): Promise<void> {
    return this.allergyService.updateAllergyEffects(this.updatedAllergy.domainId!, effects).toPromise();
  }


  isAtLeastOneFieldDifferent(): boolean {

      let isAtLeastOneFieldDifferent = false;

      if (this.checkForChangesInDesignation()) {
        isAtLeastOneFieldDifferent = true;
        this.designationDifferent = true;
        this.updatedAllergy.designation = this.updatedAllergy.designation.trim();
      }

      if (this.checkForChangesInDescription()) {
        isAtLeastOneFieldDifferent = true;
        this.descriptionDifferent = true;
        this.updatedAllergy.description = this.updatedAllergy.description.trim();
      }

      if (this.checkForChangesInEffects()) {
        isAtLeastOneFieldDifferent = true;
        this.effectsDifferent = true;
        this.updatedAllergy.effects = this.updatedAllergy.effects!.map(effect => effect.trim());
      }

      return isAtLeastOneFieldDifferent
  }

  async editAllergy() {
    this.hasError = false;

    try {
      if (this.designationDifferent) {
        await this.updateAllergyDesignation(this.updatedAllergy.designation);
      }

      if (this.descriptionDifferent) {
        await this.updateAllergyDescription(this.updatedAllergy.description);
      }

      if (this.effectsDifferent) {
        await this.updateAllergyEffects(this.updatedAllergy.effects!);
      }

      if (!this.hasError) {
        this.router.navigate(['/admin/allergyManagement']);
        alert('Allergy updated successfully.');
      }
    } catch (error) {
      this.hasError = true;
      console.error('Error updating allergy:', error);
      alert(error);
    }
  }


  checkForChangesInDesignation(): boolean {
    return this.updatedAllergy.designation.trim() !== this.originalAllergy.designation.trim() && this.updatedAllergy.designation.trim() !== '';
  }

  checkForChangesInDescription(): boolean {
    return this.updatedAllergy.description.trim() !== this.originalAllergy.description.trim() && this.updatedAllergy.description.trim() !== '';
  }

  checkForChangesInEffects(): boolean {
    return this.updatedAllergy.effects!.length !== this.originalAllergy.effects!.length || this.updatedAllergy.effects!.some(effect => !this.originalAllergy.effects!.includes(effect));
  }

  addEffect() {
    if (this.newEffect.trim()) {
      this.updatedAllergy.effects!.push(this.newEffect.trim());
      this.newEffect = '';
    }
  }

  removeEffect(index: number) {
    this.updatedAllergy.effects!.splice(index, 1);
  }
}
