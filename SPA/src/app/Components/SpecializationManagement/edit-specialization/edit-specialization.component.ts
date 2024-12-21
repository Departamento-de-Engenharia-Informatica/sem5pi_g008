import { Component, OnInit } from '@angular/core';
import { Router, ActivatedRoute } from '@angular/router';
import { SpecializationService } from '../../../services/SpecializationService/specialization.service';
import { Specialization } from '../../../Domain/Specialization';

@Component({
  selector: 'app-edit-specialization',
  templateUrl: './edit-specialization.component.html',
  styleUrls: ['./edit-specialization.component.css'],
})
export class EditSpecializationComponent implements OnInit {
  public updatedSpecialization!: Specialization;
  public originalSpecialization!: Specialization;

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private specializationService: SpecializationService
  ) {}

  ngOnInit() {
    const specializationFromState = history.state.specialization;
    if (specializationFromState) {
      this.updatedSpecialization = { ...specializationFromState };
      this.originalSpecialization = { ...specializationFromState };
    } else {
      console.error('Specialization data not found in router state.');
      alert('Error loading specialization details.');
      this.router.navigate(['/admin/specialization']);
    }
  }

  async saveChanges() {
    let isSuccessfulUpdate;

    isSuccessfulUpdate = await this.updateSpecializationName();
    if (isSuccessfulUpdate) {
      isSuccessfulUpdate = await this.updateSpecializationDescription();
    }

    // Show success message only if both updates were successful
    if (isSuccessfulUpdate) {
      alert('Specialization updated successfully.');
    }
    this.router.navigate(['/admin/specialization']);
  }

  async updateSpecializationName(): Promise<boolean> {
    if (this.updatedSpecialization?.specializationName !== this.originalSpecialization?.specializationName) {
      const id = this.originalSpecialization?.specializationID;
      const newName = this.updatedSpecialization?.specializationName;

      try {
        await this.specializationService.updateSpecializationName(id, newName).toPromise();
        return true;
      } catch (error) {
        console.error('Error occurred while updating name:', error);
        alert('An error occurred while updating the specialization name. Please try again.');
        return false; // Error
      }
    }
    return true; // No change in name, no error
  }

  async updateSpecializationDescription(): Promise<boolean> {
    if (this.updatedSpecialization?.specializationDescription !== this.originalSpecialization?.specializationDescription) {
      const id = this.originalSpecialization?.specializationID;
      const newDescription = this.updatedSpecialization?.specializationDescription;

      try {
        await this.specializationService.updateSpecializationDescription(id, newDescription).toPromise();
        return true;
      } catch (error) {
        console.error('Error occurred while updating description:', error);
        alert('An error occurred while updating the specialization description. Please try again.');
        return false; // Error
      }
    }
    return true; // No change in description, no error
  }
}
