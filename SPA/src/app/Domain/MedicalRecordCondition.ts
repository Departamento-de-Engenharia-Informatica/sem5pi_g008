export interface MedicalRecordCondition {
  conditionId: string;
  conditionCode: string;
  conditionDesignation: string;
  medicalRecordId: string;
  doctorName: string;
  doctorLicenseNumber?: number;
  comment: string;
}
