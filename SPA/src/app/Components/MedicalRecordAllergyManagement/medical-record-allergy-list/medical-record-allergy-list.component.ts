import {Component, Inject, Input, OnInit} from "@angular/core";
import {
    MedicalRecordAllergyService
} from "../../../services/medicalRecordAllergyService/medical-record-allergy.service";
import {DisplayMedicalRecordAllergyDTO} from "../../../DTOs/displayDTOs/displayMedicalRecordAllergyDTO";

@Component({
    selector: "app-medical-record-allergy-list",
    templateUrl: "./medical-record-allergy-list.component.html",
    styleUrls: ["./medical-record-allergy-list.component.css"],
})
export class MedicalRecordAllergyListComponent implements OnInit {
    constructor(
        @Inject(MedicalRecordAllergyService)
        private medicalRecordAllergyService: MedicalRecordAllergyService
    ) {
    }

    public errorMessage: string = "";
    public medicalRecordAllergies: DisplayMedicalRecordAllergyDTO[] = [];
    public filteredAllergies: DisplayMedicalRecordAllergyDTO[] = [];
    @Input() public medicalRecordId: string = "";
    public searchQuery: string = ""; // Tracks the user's search input

    ngOnInit(): void {
        this.fetchMedicalRecordConditions();
    }

    fetchMedicalRecordConditions() {
        this.medicalRecordAllergyService
            .listAllergiesInMedicalRecord(this.medicalRecordId).subscribe(
            (response) => {
                this.medicalRecordAllergies = response;
                this.filteredAllergies = [...this.medicalRecordAllergies];
            },
            (error) => {
                this.errorMessage = "Failed to load allergies.";
                console.error("Failed to load allergies:", error);
            }
        );
    }

    searchAllergy() {
        if (this.searchQuery.trim() === "") {
            this.filteredAllergies = [...this.medicalRecordAllergies];
        } else {
            this.filteredAllergies = this.medicalRecordAllergies.filter((allergy) =>
                allergy.allergy
                    .toLowerCase()
                    .includes(this.searchQuery.toLowerCase().trim())
            );
        }
    }
}
