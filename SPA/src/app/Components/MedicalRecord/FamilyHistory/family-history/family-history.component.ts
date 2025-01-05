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
      familyMember: ['', Validators.required],  // Apenas requerido
      condition: ['', Validators.required],     // Apenas requerido
    });
  }

  onSubmit(): void {
    // Verifica se os campos não estão vazios ou com apenas espaços
    if (this.familyHistoryForm.valid) {
      const familyMemberValue = this.familyHistoryForm.get('familyMember')?.value.trim();
      const conditionValue = this.familyHistoryForm.get('condition')?.value.trim();

      if (!familyMemberValue || !conditionValue) {
        alert('Family member and condition cannot be empty or just spaces.');
        return;  // Não envia o formulário se algum campo for vazio ou contiver apenas espaços
      }

      // Preenche o valor corretamente
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
      alert('Please fill out all required fields correctly.');
    }
  }

  // Função que mostra o formulário quando o botão é clicado
  toggleFormVisibility(): void {
    this.formVisible = !this.formVisible;
  }
}
