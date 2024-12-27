export interface IMedicalRecordFamilyHistoryDTO {
  medicalRecord: string;
  familyMember: string; // e.g., "Father", "Mother", "Sibling"
  condition: string; // e.g., "Diabetes", "Hypertension"
}
