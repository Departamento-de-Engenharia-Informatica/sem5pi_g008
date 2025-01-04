export interface BackendAllergyDTO{
  domainId?: number;
  code: string;
  designation: string;
  description: string;
  effects: string[];
  isDeleted?: boolean;
}
