import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {
  MedicalRecordMedicalConditionService
} from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';

@Component({
  selector: 'app-update-medical-record-condition',
  templateUrl: './update-medical-record-condition.component.html',
  styleUrls: ['./update-medical-record-condition.component.css']
})
export class UpdateMedicalRecordConditionComponent {
  updateForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private medicalRecordConditionService: MedicalRecordMedicalConditionService
  ) {
    // Inicializando o formulário com validação para o comentário
    this.updateForm = this.fb.group({
      conditionId: ['', Validators.required],
      comment: ['', Validators.required]
    });
  }

  onSubmit(): void {
    if (this.updateForm.valid) {
      const { conditionId, comment } = this.updateForm.value;

      // Enviando os dados para o serviço
      this.medicalRecordConditionService.updateComment(conditionId, comment).subscribe(
        () => {
          alert('Comment updated successfully!');
          this.updateForm.reset(); // Resetando o formulário
        },
        (error) => {
          console.error('Failed to update the comment:', error);
          alert('Error updating comment. Please try again.');
        }
      );
    } else {
      alert('Please fill out all required fields.');
    }
  }
}
