import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {Router} from '@angular/router';
import {FamilyHistoryService} from '../../../../services/MedicalRecordFamilyHistory/family-history.service';

@Component({
  selector: 'app-family-history',
  templateUrl: './family-history.component.html',
  styleUrls: ['./family-history.component.css']
})
export class FamilyHistoryComponent {
  familyHistoryForm: FormGroup;

  constructor(private fb: FormBuilder,private router: Router,private familyHistoryService: FamilyHistoryService) {
    this.familyHistoryForm = this.fb.group({
      familyMember: ['', Validators.required],
      condition: ['', Validators.required],
      comment: ['']
    });
  }

  onSubmit(): void {
    if (this.familyHistoryForm.valid) {
      console.log('Form submitted:', this.familyHistoryForm.value);

      this.familyHistoryService.saveFamilyHistory(this.familyHistoryForm.value).subscribe(
        (response: any) => {
          alert('Family history saved successfully!');
        },
        (error) => {
          console.error('Failed to load medical record conditions:', error);
          alert('There was an error saving the family history. Please try again.');
        }
      );
    } else {
      alert('Please fill out all required fields.');
    }
  }

}
