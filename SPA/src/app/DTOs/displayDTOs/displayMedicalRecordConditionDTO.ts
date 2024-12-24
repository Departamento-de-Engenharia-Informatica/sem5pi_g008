export interface DisplayMedicalRecordConditionDTO{
  conditionCode: string;
  conditionDesignation: string;
  doctorName: string;
  doctorLicenseNumber?: number;
  comment: string;
}
