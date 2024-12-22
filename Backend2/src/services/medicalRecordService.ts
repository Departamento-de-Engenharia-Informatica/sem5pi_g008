import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import IMedicalRecordDTO from "../dto/IMedicalRecordDTO";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import IMedicalRecordAllergyRepo from "./IRepos/IMedicalRecordAllergyRepo";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IAllergyRepo from "./IRepos/IAllergyRepo";


@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo,
        @Inject(config.repos.medicalRecordAllergy.name) private medicalRecordAllergyRepo: IMedicalRecordAllergyRepo,
        @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo
    ) {}

    public async createMedicalRecord(medicalRecordId:string): Promise<void> {
      const medicalProps = {};
      const medicalRecord = MedicalRecord.create(medicalProps);
      await this.medicalRecordRepo.save(medicalRecord.getValue(), medicalRecordId);
    }

    public async getAllergies(medicalRecordId:string): Promise<IMedicalRecordAllergyDTO[]> {


      const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);
      const medicalRecordPrivateId = medicalRecord.props._id.toString();

      const medicalRecordAllergies = await this.medicalRecordAllergyRepo.getByMedicalId(medicalRecordPrivateId);

      console.log(medicalRecordAllergies);

      let aux : IMedicalRecordAllergyDTO[] = [];

      for(let i = 0; i < medicalRecordAllergies.length; i++){
        const dto = await MedicalRecordAllergyMapper.toDTO(medicalRecordAllergies[i]);
        aux.push(await this.fixDto(dto));
      }

      return aux;
    }

    private async fixDto(dto: IMedicalRecordAllergyDTO): Promise<IMedicalRecordAllergyDTO> {
      const allergy = await this.allergyRepo.getById(dto.allergy);
      dto.allergy = allergy.allergy;
      dto.doctor = await this.getDoctorName(dto.doctor);
      return dto;
    }

    private async getDoctorName(doctorId: string): Promise<string> {
      return "Doctor";
    }
}

