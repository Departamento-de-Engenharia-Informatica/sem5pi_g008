export interface BackendMedicalRecordConditionDTO{
  conditionId?: string;
  conditionDesignation: string;
  medicalRecordId?: string;
  doctorName: string;
  doctorLicenseNumber?: number;
  comment: string;
}
