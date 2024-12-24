import {MedicalRecordAllergyMapper} from "../../../DTOs/mappers/medicalRecordAllergyMapper";
import {DisplayMedicalRecordAllergyDTO} from "../../../DTOs/displayDTOs/displayMedicalRecordAllergyDTO";
import {Component, Inject, Input, OnInit} from "@angular/core";
import {
    MedicalRecordAllergyService
} from "../../../services/medicalRecordAllergyService/medical-record-allergy.service";

@Component({
    selector: 'app-medical-record-allergy-list',
    templateUrl: './medical-record-allergy-list.component.html',
    styleUrl: './medical-record-allergy-list.component.css'
})
export class MedicalRecordAllergyListComponent implements OnInit {
    constructor(@Inject(MedicalRecordAllergyService) private medicalRecordAllergyService: MedicalRecordAllergyService) {}

    public errorMessage: string = "";
    public medicalRecordAllergies: DisplayMedicalRecordAllergyDTO[] = [];
    @Input() public medicalRecordId: string = "";

    ngOnInit(): void {
        this.fetchMedicalRecordConditions();
        console.log("Medical Record ID: ", this.medicalRecordId);
    }

    fetchMedicalRecordConditions() {
        this.medicalRecordAllergyService.listAllergiesInMedicalRecord(this.medicalRecordId).subscribe(
            (response) => {
                this.medicalRecordAllergies = response;
            },
            (error) => {
                this.errorMessage = 'Failed to load allergies.';
                console.error('Failed to load allergies:', error);
            }
        );
    }
}
