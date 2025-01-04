import { Component, Input } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { FamilyHistoryService } from '../../../../services/MedicalRecordFamilyHistory/family-history.service';

@Component({
  selector: 'app-family-history',
  templateUrl: './family-history.component.html',
  styleUrls: ['./family-history.component.css']
})
export class FamilyHistoryComponent {
  familyHistoryForm: FormGroup;
  @Input() public medicalRecordId: string = "";
  formVisible: boolean = false; // Controla a exibição do formulário

  constructor(
    private fb: FormBuilder,
    private router: Router,
    private familyHistoryService: FamilyHistoryService
  ) {
    this.familyHistoryForm = this.fb.group({
      familyMember: ['', Validators.required],
      condition: ['', Validators.required],
    });
  }

  onSubmit(): void {
    if (this.familyHistoryForm.valid) {
      console.log('Form submitted:', this.familyHistoryForm.value);

      this.familyHistoryService.saveFamilyHistory(this.familyHistoryForm.value, this.medicalRecordId).subscribe(
        (response: any) => {
          alert('Family history saved successfully!');
        },
        (error) => {
          console.error('Failed to save family history:', error);
          alert('There was an error saving the family history. Please try again.');
        }
      );
    } else {
      alert('Please fill out all required fields.');
    }
  }

  // Função que mostra o formulário quando o botão é clicado
  toggleFormVisibility(): void {
    this.formVisible = !this.formVisible;
  }
}
