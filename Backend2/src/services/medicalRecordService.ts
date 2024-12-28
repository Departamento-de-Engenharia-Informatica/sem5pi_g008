import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import IMedicalRecordAllergyRepo from "./IRepos/IMedicalRecordAllergyRepo";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import IMedicalRecordFreeTextRepo from "./IRepos/IMedicalRecordFreeTextRepo";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordFreeTextMap} from "../mappers/MedicalRecordFreeTextMapper";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import IStaffDetailsDTO from "../dto/IStaffDetailsDTO";
import http from "node:http";
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import {
    MedicalConditionNotFoundException
} from "../domain/MedicalCondition/Exceptions/MedicalConditionNotFoundException";
import {
    MedicalRecordConditionNotFoundException
} from "../domain/MedicalRecordCondition/MedicalRecordConditionNotFoundException";
import {Code} from "../domain/MedicalCondition/code";
import {Designation} from "../domain/MedicalCondition/designation";



@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo,
        @Inject(config.repos.medicalRecordAllergy.name) private medicalRecordAllergyRepo: IMedicalRecordAllergyRepo,
        @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo,
        @Inject(config.repos.medicalRecordFreeText.name) private medicalRecordFreeTextRepo: IMedicalRecordFreeTextRepo,
        @Inject(config.repos.medicalRecordCondition.name) private medicalRecordConditionRepo: IMedicalRecordConditionRepo,
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo

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
      dto.allergy = allergy.designation;
      const doctor = await this.getStaffDetails(dto.doctor);
      dto.doctor = doctor.firstName + " " + doctor.lastName;
      return dto;
    }


    public async getMedicalRecordConditions(medicalRecordId: string): Promise<IMedicalRecordConditionDTO[]> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const medicalRecordConditionList = await this.medicalRecordConditionRepo.getMedicalRecordConditionsWithIds(medicalRecord.props._id.toString());

        if (medicalRecordConditionList.length === 0) {
            throw new NoMedicalRecordConditionsException();
        }

        const medicalRecordConditionDTOList = [];

        for (const medicalRecordCondition of medicalRecordConditionList) {

            const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

            const condition = await this.medicalConditionRepo.getMedicalConditionByBusinessId(medicalRecordCondition.conditionId);

            const medicalConditionDTO = MedicalRecordConditionMapper.toDTO(medicalRecordCondition, condition, medicalRecord.id, staffDetailsDTO);

            medicalRecordConditionDTOList.push(medicalConditionDTO);
        }

        return medicalRecordConditionDTOList;
    }

    public async getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const code = Code.create(conditionCode);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByCode(code.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Code.");
        }

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Code.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    public async getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const designation = Designation.create(conditionDesignation);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByDesignation(designation.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Designation.");
        }

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Designation.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    private async getStaffDetails(staffId: string): Promise<IStaffDetailsDTO> {

        const url = config.Backend1.URL + '/Staff';
        const urlApi = `${url}/by-id/${staffId}`;

        return new Promise((resolve, reject) => {
            http.get(urlApi, (res) => {
                let data = '';

                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {

                    try {
                        const staff: IStaffDetailsDTO = JSON.parse(data);
                        resolve(staff);
                    } catch (error) {
                        resolve(undefined);
                    }
                });
            }).on('error', (error) => {
                reject(error);
            });
        });
    }


    public async addFreeText(medicalRecord: any): Promise<any> {

        const medicalRecordDomain=MedicalRecordFreeTextMap.toDomain(medicalRecord);
        await this.medicalRecordFreeTextRepo.save(medicalRecordDomain);

    }

}



